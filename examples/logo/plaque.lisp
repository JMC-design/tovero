#| plaque.lisp

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


#|
This is a sample showing applying a texture to a model. If simage library
is installed in non-standard location, need to set environment variable:

  export COIN_SIMAGE_LIBNAME=<full-path-to>/libsimage.so

|#

(require 'clive-viewer)
(require 'tovero)

(tovero:use-tovero-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clive:define-scene-type separator)
  (clive:define-scene-type trackball-manip)
  (clive:define-scene-type material)
  (clive:define-scene-type texture2)
  (clive:define-scene-type indexed-face-set)
  (clive:define-scene-type vertex-property))

(defun create-model-mesh ()
  (let* ((rad-x 350)
         (y-stretch 1.18)
         (thickness (/ rad-x 10))
         (edge-radius (/ thickness 2))
         (edge-base-pt #[0 0 (- thickness edge-radius)])
         (out-edge-torus
          (torus (- rad-x edge-radius) edge-radius edge-base-pt))
         (out-edge-cyl
          (difference
           (cylinder rad-x edge-radius edge-base-pt)
           (cylinder (- rad-x edge-radius) edge-radius edge-base-pt)))
         (plaque
          (scale-y
           (difference
            (cylinder rad-x thickness)
            (difference out-edge-cyl out-edge-torus))
           y-stretch)))
    (shape-to-mesh
     plaque
     (region3 '((-500 . 500) (-500 . 500) (-500 . 500)))
     0.3)))  ; .1 low-res, .3 high-res

(defun main-thread ()
  (format t "~% *** Creating model, please wait...")
  (let* ((mesh (create-model-mesh))
         (model (clive:separator
                 ((clive:texture2 :filename "woodgrain.png" )
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
