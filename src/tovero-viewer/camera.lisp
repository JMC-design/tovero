#| camera.lisp

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

;;; class rotation
(defclass rotation ()
  ((rotatingp :reader is-rotating-p :initform nil)
   (screen-rot-x :reader get-screen-rot-x :initform 0)
   (screen-rot-y :reader get-screen-rot-y :initform 0)
   (screen-base-x)
   (screen-base-y)
   (last-screen-offset-x :initform 0)
   (last-screen-offset-y :initform 0)))
(defgeneric rotation-start (self x y))
(defmethod rotation-start ((self rotation)
                           (x integer)
                           (y integer))
  (setf (slot-value self 'screen-base-x) x)
  (setf (slot-value self 'screen-base-y) y)
  (setf (slot-value self 'rotatingp) t))

(defparameter *rotation-scale* 360/500)

(defgeneric rotation-rotate (self x y))
(defmethod rotation-rotate ((self rotation)
                            (x integer)
                            (y integer))
  (let ((x-prime (+ (- x (slot-value self 'screen-base-x))
                    (slot-value self 'last-screen-offset-x)))
        (y-prime (+ (- y (slot-value self 'screen-base-y))
                    (slot-value self 'last-screen-offset-y))))
    (setf (slot-value self 'screen-rot-y) (* *rotation-scale* x-prime))
    (setf (slot-value self 'screen-rot-x) (* *rotation-scale* y-prime))))

(defmethod rotation-end (self x y))
(defmethod rotation-end ((self rotation)
                         (x integer)
                         (y integer))
  (setf (slot-value self 'rotatingp) nil)
  (setf (slot-value self 'last-screen-offset-x)
             (+ (slot-value self 'last-screen-offset-x)
                (- x (slot-value self 'screen-base-x))))
  (setf (slot-value self 'last-screen-offset-y)
             (+ (slot-value self 'last-screen-offset-y)
                (- y (slot-value self 'screen-base-y)))))

;; class zoom
(defclass zoom ()
  (;; current zoom
   (zoom-value :reader get-zoom :initform 1 :initarg :zoom-value)
   ;; how fast we zoom
   (factor :initarg :factor)))

(defgeneric zoom-in (self))
(defmethod zoom-in ((self zoom))
  (let* ((factor (slot-value self 'factor))
         (zoom-value (+ (slot-value self 'zoom-value) factor)))
    (setf (slot-value self 'zoom-value) zoom-value)))

(defgeneric zoom-out (self))
(defmethod zoom-out ((self zoom))
  (let* ((factor (slot-value self 'factor))
         (zoom-value (- (slot-value self 'zoom-value) factor)))
    (setf (slot-value self 'zoom-value) zoom-value)))

;;; class camera
(defclass camera ()
  ((width :reader get-screen-width :writer set-screen-width :initform 0)
   (height :reader get-screen-height :writer set-screen-height :initform 0)
   (eye :reader get-eye :writer set-eye :initform #(0 0 1))
   (center :reader get-center :writer set-center :initform #(0 0 0))
   (up :reader get-up :writer set-up :initform #(0 1 0))
   (rotator :reader get-rotator :initform (make-instance 'rotation))
   (zoomer :reader get-zoomer
           :initform (make-instance 'zoom :factor 0.05 :zoom-value 1))))

(defun vec3-length (v)
  (flet ((square (e) (* e e)))
    (sqrt (+ (square (elt v 0))
             (square (elt v 1))
             (square (elt v 2))))))
(defun vec3-sub (v1 v2)
  (vector (- (elt v1 0)
             (elt v2 0))
          (- (elt v1 1)
             (elt v2 1))
          (- (elt v1 2)
             (elt v2 2))))

(defgeneric establish-modelview-matrix (self))
(defmethod establish-modelview-matrix ((self camera))
  (let ((eye (slot-value self 'eye))
        (center (slot-value self 'center))
        (up (slot-value self 'up)))
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (let ((distance (vec3-length (vec3-sub eye center)))
          (rotator (slot-value self 'rotator)))
      (gl:translate 0 0 (- distance))
      (gl:rotate (slot-value rotator 'screen-rot-x) 1 0 0)
      (gl:rotate (slot-value rotator 'screen-rot-y) 0 1 0)
      (gl:translate 0 0 distance)
      (cl-glu:look-at (elt eye 0)
                      (elt eye 1)
                      (elt eye 2)
                      (elt center 0)
                      (elt center 1)
                      (elt center 2)
                      (elt up 0)
                      (elt up 1)
                      (elt up 2)))))

(defun bounds-length (l)
  (let* ((x (car l))
         (x-min (car x))
         (x-max (cdr x))
         (y (cadr l))
         (y-min (car y))
         (y-max (cdr y))
         (z (caddr l))
         (z-min (car z))
         (z-max (cdr z)))
    (vec3-length (vector (- x-max x-min)
                         (- y-max y-min)
                         (- z-max z-min)))))

(defgeneric establish-projection-matrix (self bounds))
(defmethod establish-projection-matrix ((self camera)
                                        (bounds list))
  (let* ((r (bounds-length bounds))
         (dist-2-eye (vec3-length (vec3-sub (slot-value self 'eye)
                                            (slot-value self 'center))))
         (viewport-aspect (/ (slot-value self 'height)
                             (slot-value self 'width)))
         (zoom (slot-value (slot-value self 'zoomer) 'zoom-value))
         (min (vector (- r) (- r) (- r)))
         (min-x (elt min 0))
         (min-y (elt min 1))
         (max (vector r r r))
         (max-x (elt max 0))
         (max-y (elt max 1))
         (window-aspect (/ (- max-y min-y) (- max-x min-x))))
    ;; calculate viewport parameters
    (if (> viewport-aspect window-aspect)
        ;; viewport taller than it needs to be
        (let ((new-height (* viewport-aspect (- max-x min-x)))
              (y-mid (/ (+ min-y max-y) 2)))
          (setf (elt max 1) (+ y-mid (* 1/2 new-height)))
          (setf (elt min 1) (- y-mid (* 1/2 new-height))))
        ;; viewport wider than it needs to be
        (let ((new-width (/ (- max-y min-y) viewport-aspect))
              (x-mid (/ (+ min-x max-x) 2)))
          (setf (elt max 0) (+ x-mid (* 1/2 new-width)))
          (setf (elt min 0) (- x-mid (* 1/2 new-width)))))
    ;; do the projection
    (let ((new-min (vector (/ (elt min 0) zoom)
                           (/ (elt min 1) zoom)
                           (elt min 2)))
          (new-max (vector (/ (elt max 0) zoom)
                           (/ (elt max 1) zoom)
                           (elt max 2))))
      ;; use an orthographic projection using adjusted model boundaries
      (gl:ortho (elt new-min 0) (elt new-max 0)
                (elt new-min 1) (elt new-max 1)
                (elt new-min 2) (elt new-max 2)))))
