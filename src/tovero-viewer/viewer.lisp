#| viewer.lisp

Copyright 2018 Kavalogic, Inc.

This file is part of Tovero.

Tovero is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Tovero is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
for more details.  You should have received a copy of the GNU General
Public License along with Tovero. If not, see
<http://www.gnu.org/licenses/>.

|#

(in-package #:tovero-viewer)

;;; Uncomment the following for debug messages:
;;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;;   (pushnew :debug-messages *features*))

;;; Globals
(defparameter *full-screen-p* nil)
;;;   object buffers
(defparameter *vertex-buffer* 0)
(defparameter *index-buffer* 0)
;;;   shape
(defparameter *shape-changed-p* nil)
;;;     set by external thread, used when shape is changed
(defparameter *new-mesh* nil)
(defparameter *new-bounds* nil)
;;;     set by viewer thread, used to display
(defparameter *mesh* nil)
(defparameter *bounds* nil)
;;;   mutexes
(defparameter *run-mutex* nil)
(defparameter *shape-mutex* nil)
;;;   other
(defparameter *main-window* nil)
(defparameter *camera* nil)
(defparameter *shader* nil)

(defun print-help ()
  (format t "~%Viewer Keys:~%")
  (format t "------------~%")
  (format t "General::~%")
  (format t "  'h' -- Print this help information~%")
  (format t "  ESC -- Exit viewer~%"))

(defun draw-shape ()
  (unless (null *mesh*)
    (gl:enable-client-state :vertex-array)
    (gl:bind-buffer :array-buffer *vertex-buffer*)
    (gl/vertex-pointer 3 :float)
    (gl:bind-buffer :element-array-buffer *index-buffer*)

    (gl/draw-elements :triangles
                      (* 3 (tovero:get-triangle-count *mesh*))
                      :unsigned-int)

    (gl:disable-client-state :vertex-array)
    (gl:disable-client-state :index-array)))

(defun cleanup-for-exit ()
  (cleanup *shader*)
  (set-shape nil nil nil))

;; GLUT display callback
(defun default-display-handler ()
  (gl:clear :color-buffer :depth-buffer)

  (establish-modelview-matrix *camera*)

  (draw-shape)

  (glut:swap-buffers))
(export 'default-display-handler)
(cffi:defcallback display :void
  ()
  #+debug-messages (format t "glut 'display' callback~%")
  (default-display-handler))

;;; GLUT reshape callback
(defun reshape ()
  ;; use the whole window
  (let ((width (get-screen-width *camera*))
        (height (get-screen-height *camera*)))
    (gl:viewport 0 0 width height))

  ;; update the projection matrix
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (unless (null *bounds*)
    (establish-projection-matrix *camera* *bounds*))

  (gl:matrix-mode :modelview))

(defun default-reshape-handler (width height)
  ;; update the camera
  (set-screen-width width *camera*)
  (set-screen-height height *camera*)

  (reshape))
(export 'default-reshape-handler)
(cffi:defcallback reshape :void
  ((width :int)
   (height :int))
  #+debug-messages
  (format t "glut 'reshape' callback (width: ~a height ~a)~%" width height)
  (default-reshape-handler width height))

;;; GLUT mouse callback
(defun default-mouse-handler (button state x y
                              was-shift-down-p was-ctrl-down-p was-alt-down-p)
  (let ((button-keyword (foreign-enum-keyword 'glut:mouse-button button)))
    (case button-keyword
      (;; left button rotates
       :left-button
       (let ((rotator (get-rotator *camera*))
             (state-keyword (foreign-enum-keyword 'glut:mouse-button-state state)))
         (if (eq state-keyword :down)
             (rotation-start rotator x y)
             (rotation-end rotator x y))))
      (;; mouse wheel button rotate upwards
       :wheel-up
       (let ((zoomer (get-zoomer *camera*)))
         (zoom-in zoomer)
         (reshape)
         (glut:post-redisplay)))
      (;; mouse wheel button rotate downwards
       :wheel-down
       (let ((zoomer (get-zoomer *camera*)))
         (zoom-out zoomer)
         (reshape)
         (glut:post-redisplay))))))
(export 'default-mouse-handler)
(cffi:defcallback mouse :void
  ((button :int)
   (state :int)
   (x :int)
   (y :int))
  #+debug-messages
  (format t "glut 'mouse' callback (button: ~a state: ~a x: ~a y: ~a)~%"
          button state x y)
  (multiple-value-bind (shiftp ctrlp altp)
      (glut:get-modifier-values)
    (default-mouse-handler button state x y
                           shiftp ctrlp altp)))

;;; GLUT motion callback
(defun default-motion-handler (x y)
  (let ((rotator (get-rotator *camera*)))
    (if (is-rotating-p rotator)
        (progn
          ;; update rotation
          (rotation-rotate rotator x y)
          (glut:post-redisplay)))))
(export 'default-motion-handler)
(cffi:defcallback motion :void
  ((x :int)
   (y :int))
  #+debug-messages
  (format t "glut 'motion' callback (x: ~a y: ~a)~%"
          x y)
  (default-motion-handler x y))

;;; GLUT visibility callback
(defun default-visibility-handler (visibility) nil)
(export 'default-visibility-handler)
(cffi:defcallback visibility :void ((visibility glut:visibility-state))
  (default-visibility-handler visibility))

(defun make-mutex ()
  #+sbcl (sb-thread:make-mutex)
  #+ecl (mp:make-lock))

(defun unlock-mutex (mutex)
  #+sbcl
  (sb-sys:without-interrupts
      (sb-thread:release-mutex mutex :if-not-owner :force))
  #+ecl
  (mp:giveup-lock mutex))

;;; tries to acquire the mutex without waiting,
;;; and if aquired, evaluates body
(defmacro with-try-mutex (mutex &body body)
  #+sbcl
  `(sb-thread:with-mutex (,mutex :wait-p nil) ,@body)
  #+ecl
  `(let ((got-lock-p nil))
     (mp:without-interrupts
         (unwind-protect
              (let ((si:*interrupts-enabled* nil))
                (setf got-lock-p (mp:get-lock ,mutex nil))
                (locally ,@body))
           (when got-lock-p (mp:giveup-lock ,mutex))))))

(defmacro with-mutex (mutex &body body)
  #+sbcl
  `(sb-thread:with-mutex (,mutex) ,@body)
  #+ecl
  `(mp:with-lock (,mutex) ,@body))

(defun try-mutex (mutex)
  #+sbcl
  (sb-sys:without-interrupts
    (sb-sys:allow-with-interrupts (sb-thread:grab-mutex mutex :waitp nil)))
  #+ecl
  (mp:get-lock mutex))

(defun setup-for-display ()
  (let ((need-redisplay-p nil))
    (with-try-mutex *shape-mutex*
      (when *shape-changed-p*
        (setf *mesh* *new-mesh*)
        (setf *bounds* *new-bounds*)
        (setf *new-mesh* nil)
        (setf *new-bounds* nil)
        (setf *shape-changed-p* nil)
        (setf need-redisplay-p t)))
    (when need-redisplay-p
      (let ((vertices (tovero:get-vertices *mesh*))
            (vertex-count (tovero:get-vertex-count *mesh*))
            (indices (tovero:get-triangle-indices *mesh*))
            (index-count (* 3 (tovero:get-triangle-count *mesh*))))
        ;; set vertex buffer
        (gl:delete-buffers (list *vertex-buffer*)) ; no-op if 0
        (setf *vertex-buffer* (elt (gl:gen-buffers 1) 0))
        (gl:bind-buffer :array-buffer *vertex-buffer*)
        (gl/buffer-data :array-buffer
                        (* 3 (foreign-type-size :float) vertex-count)
                        vertices
                        :static-draw)
        ;; set index buffer
        (gl:delete-buffers (list *index-buffer*)) ; no-op if 0
        (setf *index-buffer* (elt (gl:gen-buffers 1) 0))
        (gl:bind-buffer :element-array-buffer *index-buffer*)
        (gl/buffer-data :element-array-buffer
                        (* (foreign-type-size :uint32) index-count)
                        indices
                        :static-draw)
        ;;; unbind buffers
        (gl:bind-buffer :array-buffer 0)
        (gl:bind-buffer :element-array-buffer 0)))
    need-redisplay-p))

(defun exit-viewer ()
  (unless (null *main-window*) (glut:destroy-window *main-window*)))

;;; GLUT keyboard callback
(defun default-keyboard-handler (key x y)
  (case (code-char key)
      (#\f
       (setf *full-screen-p* (not *full-screen-p*))
       (glut:full-screen *main-window* *full-screen-p*))
      ((#\Esc #\q)
       (exit-viewer))
      (#\]
       (let ((zoomer (get-zoomer *camera*)))
         (zoom-in zoomer)
         (reshape)
         (glut:post-redisplay)))
      (#\[
        (let ((zoomer (get-zoomer *camera*)))
          (zoom-out zoomer)
          (reshape)
          (glut:post-redisplay)))))
(export 'default-keyboard-handler)
(cffi:defcallback keyboard :void
  ((key :unsigned-char)
   (x :int)
   (y :int))
  #+debug-messages
  (format t "glut 'keyboard' callback (~a x: ~a y: ~a)~%"
          key x y)
  (default-keyboard-handler key x y))

;;; GLUT idle callback
(cffi:defcallback idle :void
    ()
  (when (setup-for-display)
    (reshape)
    (glut:post-redisplay)))

(defun setup-glut-callbacks ()
  (glut:display-func (cffi:get-callback 'display))
  (glut:reshape-func (cffi:get-callback 'reshape))
  (glut:mouse-func (cffi:get-callback 'mouse))
  (glut:motion-func (cffi:get-callback 'motion))
  (glut:keyboard-func (cffi:get-callback 'keyboard))
  (glut:visibility-func (cffi:get-callback 'visibility))
  (glut:idle-func (cffi:get-callback 'idle)))

(defun run-main (args)
  ;; initialize GLUT
  (glut:init "ToveroViewer")
  (glut:init-display-mode :double :rgba :alpha :depth)
  ;; create an GLUT window
  (let ((window-width (car args))
        (window-height (cadr args)))
    (glut:init-window-size 640 480))

  (setf *main-window* (glut:create-window "Tovero Viewer"))
  (setf *camera* (make-instance 'camera))
  (setf *shader* (shader))
  (setup-glut-callbacks)

  (gl:clear-color 0 0 0 1)
  (gl:clear-depth 1)
  (gl:enable :depth-test)

  (setup-for-display)

  ;; finish setting up
  (glut:set-action-on-window-close :action-glutmainloop-returns)

  ;; enter the GLUT main loop
  (glut:main-loop)

  (cleanup-for-exit)

  (unlock-mutex *run-mutex*))

(defun viewer-main (args)
  (when (null *run-mutex*)
    (setf *run-mutex* (make-mutex))
    (setf *shape-mutex* (make-mutex)))
  (if (not (try-mutex *run-mutex*))
      (error "The viewer is already running.")
      (run-main args)))

(defun set-shape (new-shape new-bounds new-resolution)
  "set-shape shape bounds resolution
   Sets the shape to display.  When the shape parameter is nil, this
   function deallocates the current mesh."
  (with-mutex *shape-mutex*
    (if (null new-shape)
        (progn
          (setf *new-mesh* nil)
          (setf *new-bounds* nil))
        (progn
          (setf *new-mesh*
                (tovero:shape-to-mesh new-shape new-bounds new-resolution
                                      :mesh-type :tovero-mesh-contiguous))
          (setf *new-bounds* new-bounds)))
    (setf *shape-changed-p* t)))
(export 'set-shape)

(defun run-new-thread (thunk)
  #+sbcl
  (sb-thread:make-thread thunk :name "viewer-thread")
  #+ecl
  (mp:process-run-function 'viewer-thread thunk))

;;; TODO: make multiple viewers possible (using a class)
;;; TODO: for now, only one viewer allowed
(defun make-viewer (&optional args)
  (run-new-thread
   (lambda ()
     (viewer-main '(640 480)))))
(export 'make-viewer)
