#| horseshoe.lisp

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

(require 'clive-viewer)
(require 'tovero)

(tovero:use-tovero-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clive:define-scene-type separator)
  (clive:define-scene-type trackball-manip)
  (clive:define-scene-type material)
  (clive:define-scene-type indexed-face-set)
  (clive:define-scene-type vertex-property))

(defun place-in-groove (object factor angl)
  (scale-y (rotate-z object angl) factor))

(defun create-model-mesh ()
  (let* ((out-rad-x 250)
         (in-rad-x 170)
         (end-radius (* (- out-rad-x in-rad-x) .4))
         (end-x-off 174)
         (end-y-off 206)
         (groove-out-x 220)
         (groove-in-x 205)
         (center-groove (/ (+ groove-out-x groove-in-x) 2))
         (hole-rad (/ (- groove-out-x groove-in-x) 2))
         (y-stretch 1.14)
         (thickness (/ out-rad-x 6))
         (groove-depth (/ thickness 2))
         (pi/4 (/ pi 4))
         (3/4pi (* pi 0.75))
         (cutout-offset (/ out-rad-x 5))
         (cutout-length (* cutout-offset 6))
         (cut-out-top
          (rotate-z (rectangle #[0 0] #[cutout-length cutout-length]) pi/4))
         (cut-out-box
          (rotate-z (box #[0 0 0] #[cutout-length cutout-length cutout-length])
                    pi/4))
         (hole-angle (/ pi 12))
         (shoe-hole (move
                     (scale-y
                      (cylinder hole-rad (* thickness 2) #[0 0 (- thickness)])
                      y-stretch)
                     #[center-groove 0 0]))
         ;; shapes for making rounded edge on top surface
         (edge-radius (/ thickness 5))
         (edge-base-pt #[0 0 (- thickness edge-radius)])
         (out-edge-torus
          (scale-y (torus (- out-rad-x edge-radius) edge-radius edge-base-pt)
          y-stretch))
         (out-edge-cyl
          (scale-y
           (difference
            (cylinder out-rad-x edge-radius edge-base-pt)
            (cylinder (- out-rad-x edge-radius) edge-radius edge-base-pt))
           y-stretch))
         (in-edge-torus
          (scale-y (torus (+ in-rad-x edge-radius) edge-radius edge-base-pt)
          y-stretch))
         (in-edge-cyl
          (scale-y
           (difference
            (cylinder (+ in-rad-x edge-radius) edge-radius edge-base-pt)
            (cylinder in-rad-x edge-radius edge-base-pt))
           y-stretch))
         (end-edge-torus
          (torus (- end-radius edge-radius) edge-radius edge-base-pt))
         (end-edge-cyl
          (difference
           (cylinder end-radius edge-radius edge-base-pt)
           (cylinder (- end-radius edge-radius) edge-radius edge-base-pt)))
         (sqrt2-edg-rad (* (sqrt 2) edge-radius))
         (pre-top-cyl
          (rotate-x (cylinder edge-radius cutout-length) (/ pi 2)))
         (top-cyl-move-vec #[0
                             (- cutout-offset sqrt2-edg-rad)
                             (- thickness edge-radius)])
         (top-left-cyl
          (scale-y
           (move (rotate-z pre-top-cyl (- 3/4pi)) top-cyl-move-vec)
           y-stretch))
         (top-right-cyl
          (scale-y
           (move (rotate-z pre-top-cyl 3/4pi) top-cyl-move-vec)
           y-stretch))
         (top-edge-box
          (move (scale-y (move cut-out-box #[0 cutout-offset 0]) y-stretch)
                #[0 (- sqrt2-edg-rad) (- thickness edge-radius)]))
         ;; main horse-shoe shapes
         (horse-shoe-base
          (difference
           (extrude-z
            (scale-y
             (difference
              (circle out-rad-x)
              (circle in-rad-x)
              (move cut-out-top #[0 cutout-offset 0]))
             y-stretch)
            0 thickness)
           (difference out-edge-cyl out-edge-torus)
           (difference in-edge-cyl in-edge-torus)
           (difference top-edge-box top-left-cyl top-right-cyl)))
         (shoe-end
          (difference
           (cylinder end-radius thickness)
           (difference end-edge-cyl end-edge-torus)))
         (horse-shoe-groove
          (scale-y
           (difference
            (cylinder groove-out-x thickness #[0 0 groove-depth])
            (cylinder groove-in-x thickness #[0 0 groove-depth])
            (move cut-out-box #[0 (- (* cutout-offset 2)) 0])
            (move cut-out-box #[0 (- (* cutout-offset 10)) 0]))
           y-stretch))
         (horse-shoe
          (union
           (difference horse-shoe-base
                       horse-shoe-groove
                       (place-in-groove shoe-hole y-stretch hole-angle)
                       (place-in-groove shoe-hole y-stretch (- hole-angle))
                       (place-in-groove shoe-hole y-stretch (- (* hole-angle 3)))
                       (place-in-groove shoe-hole y-stretch (- (* hole-angle 9)))
                       (place-in-groove shoe-hole y-stretch (- (* hole-angle 11)))
                       (place-in-groove shoe-hole y-stretch (- (* hole-angle 13))))
           (move shoe-end #[(- end-x-off) end-y-off 0])
           (move shoe-end #[end-x-off end-y-off 0]))))
         (shape-to-mesh
          horse-shoe
          (region3 '((-500 . 500) (-500 . 500) (-500 . 500)))
          #| 0.5 |# 0.5)))  ; .1 low-res, .5 hight-res

(defun main-thread ()
  #+linux (cffi:foreign-funcall "fedisableexcept" :int -1)
  (format t "~% *** Creating model, please wait...")
  (let* ((mesh (create-model-mesh))
         (model (clive:separator
                 ((clive:material :ambient-color #(.098 .085 .079)
                                  :diffuse-color #(.392 .339 .317))
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
    (clive-viewer:setup-main-loop :app-name "Model" :window-title "Model")
    ;; run viewer's loop
    (clive-viewer:run-main-loop)))

(defun main () (clive-viewer:run-thread #'main-thread '()))
