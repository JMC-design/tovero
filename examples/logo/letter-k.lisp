#| letter-k-model.lisp

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

(defun create-model-mesh ()
  (let* ((neg-pi/2 (- (/ pi 2)))
         (letter-height 200)
         (part-radius (/ letter-height 18))
         (torus-radius (/ letter-height 7))
         (side-length (* letter-height .57))
         (center-height (* torus-radius .4))
         (left-part
          (move
           (rotate-x (cylinder part-radius letter-height) neg-pi/2)
           #[0 (- (/ letter-height 2)) 0]))
         (edge-box-side 100)
         (edge-y-shift (/ edge-box-side (sqrt 2)))
         (edge-box
          (move
           (rotate-z (box #[0 0 0] #[edge-box-side edge-box-side edge-box-side])
                     (/ pi 4))
           #[0 0 -50]))
         (torus-piece
          (difference
           (rotate-x (torus torus-radius part-radius #[torus-radius 0 0]) 0)
           (move edge-box #[torus-radius 0 0])
           (move (rotate-z edge-box pi) #[torus-radius 0 0])
           (box #[torus-radius (- torus-radius) (- torus-radius)]
                #[(* torus-radius 3) torus-radius torus-radius])))
         (straight-piece
          (rotate-x (cylinder part-radius side-length) neg-pi/2))
         (right-part
          (union
           torus-piece
           (difference
            (union
             (move
              (rotate-z straight-piece (- (* pi 0.75)))
              #[0 (- center-height) 0])
             (move
              (rotate-z straight-piece (- (* pi 0.25)))
              #[0 center-height 0]))
            (move edge-box #[-45 (- edge-y-shift) 0]))))
         (brand-k
          (union (move left-part #[(- (* part-radius 0.63)) 0 0])
                 right-part)))
    (shape-to-mesh
     brand-k
     (region3 '((-300 . 300) (-300 . 300) (-300 . 300)))
     0.5)))  ; .3 low-res, .5 high-res

(defun main-thread ()
  (format t "~% *** Creating model, please wait...")
  (let* ((mesh (create-model-mesh))
         (model (clive:separator
                 ((clive:material :ambient-color (clive:color :v #(.034 .019 .019))
                                  :diffuse-color (clive:color :v #(.268 .150 .150)))
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
