#| contoured-sphere.lisp

Copyright 2018 Kavalogic, Inc.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

  (1) Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  (2) Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in
  the documentation and/or other materials provided with the
  distribution.

  (3) The name of the author may not be used to
  endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

|#

;;; Renders a sphere to line contours and shows it in the
;;; Clive Viewer.

(require 'clive-viewer)
(require 'tovero)

(tovero:use-tovero-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clive:define-scene-type separator)
  (clive:define-scene-type material)
  (clive:define-scene-type line-set)
  (clive:define-scene-type draw-style)
  (clive:define-scene-type vertex-property))

(defun make-contoured-sphere ()
  (let* ((radius 0.5)
         (s (sphere radius))
         (slice-count 10)
         (delta-z (/ (* 2 radius) slice-count))
         (bounds '((-2 . 2) (-2 . 2)))
         (resolution 10)
         (base-z (- radius))
         (sphere (clive:separator ())))
    (dotimes (i slice-count)
      (let* ((slice (tovero:shape-slice s bounds base-z resolution))
             (contour-count (tovero:get-count slice)))
        (dotimes (j contour-count)
          (let* ((contour (tovero:get-contour slice j))
                 (point-count (tovero:get-count contour))
                 (vertices
                  (clive:vertex-property
                   :vertex (clive:foreign-vertex-array
                            point-count
                            (tovero:get-points contour))))
                 (line-set (clive:line-set :num-vertices point-count
                                           :vertex-property vertices)))
            (clive:add-child sphere vertices)
            (clive:add-child sphere line-set)))
        (setf base-z (+ base-z delta-z))))
    sphere))

(defun main-thread ()
  (tovero-initialize)
  (format t "~% *** Creating model, please wait...")
  (let* ((sphere (make-contoured-sphere))
         (model (clive:separator
                 ((clive:material :ambient-color #(.01 .2 .01)
                                  :diffuse-color #(.2 .8 .2)
                                  :shininess 0.0)
#|
                  (clive:draw-style :style :draw-style-lines)
|#
                  sphere))))
    (format t "model created. ***~%")
    (clive-viewer:set-model model)
    (clive-viewer:setup-main-loop :app-name "ContouredSphere"
                                  :window-title "Contoured Sphere")
    ;; run viewer's loop
    (clive-viewer:run-main-loop)))

(defun main () (clive-viewer:run-thread #'main-thread '()))
