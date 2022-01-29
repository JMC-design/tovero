#| menger.lisp

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

From the libfive source file: ./studio/examples/menger.io
Original license: GPL 2+

|#

(require 'clive-viewer)
(require 'tovero)

(tovero:use-tovero-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clive:define-scene-type separator)
  (clive:define-scene-type material)
  (clive:define-scene-type indexed-face-set)
  (clive:define-scene-type vertex-property)
  (clive:define-scene-type draw-style))

(defun recurse (center scale i)
  (let ((base (rectangle (- center scale) (+ center scale))))
    (if (= i 0)
        base
        (apply
         #'union
         (cons base
               (map 'list
                    (lambda (o)
                      (recurse (+ center (* 2 o scale))
                               (/ scale 3)
                               (1- i)))
                    (list
                     #[1 0] #[-1 0]
                     #[0 1] #[0 -1]
                     #[1 1] #[1 -1]
                     #[-1 1] #[-1 -1]
                     ))))
        )))

(defun make-menger-mesh ()
  (let* ((region '((-10 . 10) (-10 . 10) (-10 . 10)))
         (resolution 10)
         ;; Build the 2D cutout
         (cutout (recurse #[0 0] 2/3 2))
         (menger-shape
          ;; Then reflect and cut it from a cube on all axes
          (difference (box #[-2 -2 -2] #[2 2 2])
                      cutout
                      (reflect-yz cutout)
                      (reflect-xz cutout)))
         (shape-mesh (shape-to-mesh menger-shape
                                    region
                                    resolution)))
    shape-mesh))
(defun main-thread ()
  (tovero-initialize)
  (format t "~% *** Creating model, please wait...")
  (let* ((mesh (make-menger-mesh))
         (model (clive:separator
                 ((clive:material)
#|
                  (clive:draw-style :style :draw-style-lines)
|#
                  (clive:vertex-property
                   :vertex (clive:foreign-vertex-array
                            (tovero:get-vertex-count mesh)
                            (tovero:get-vertices mesh)))
                  (clive:indexed-face-set
                   :coord-index (clive:foreign-coord-index-array
                                 (tovero:get-coord-index-count mesh)
                                 (tovero:get-coord-indices mesh)))))))
    (format t "model created. ***~%")
    (clive-viewer:set-model model)
    (clive-viewer:setup-main-loop :app-name "Menger" :window-title "Menger")
    ;; run viewer's loop
    (clive-viewer:run-main-loop)))

(defun main () (clive-viewer:run-thread #'main-thread '()))
