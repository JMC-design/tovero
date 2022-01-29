#| kinetophone.lisp

Copyright 2018 Kavalogic, Inc.
Copyright 2012-2014 Roan Trail, Inc.

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

Derived from the (Roan Trail) Tovero 0.2.0 file:
  ./src/tovero/graphics/models/kinetophone.rb

Original License: BSD 3-clause

|#

(require 'clive-viewer)
(require 'tovero)

(tovero:use-tovero-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clive:define-scene-type separator)
  (clive:define-scene-type material)
  (clive:define-scene-type indexed-face-set)
  (clive:define-scene-type vertex-property))

(defun make-kinetophone-mesh ()
  (let* ((torus-base #[0 0 5])
         (base (cylinder-vec 1.3 #[0 0 2]))
         (sound-tor1 (torus-vec #[1 0 0] 3 0.2 torus-base))
         (sound-tor2 (torus-vec #[1 0 0] 2 0.2 torus-base))
         (sound-tor3 (torus-vec #[1 0 0] 1 0.2 torus-base))
         (sound-cone (truncated-cone-vec 0.1 5.0 #[0 0 4] torus-base))
         (sound-lines  (union
                        (intersection sound-tor1 sound-cone)
                        (intersection sound-tor2 sound-cone)
                        (intersection sound-tor3 sound-cone)))
         (outer-cone (truncated-cone-vec 1.3 2.3 #[0 0 2] #[0 0 2]))
         (inner-cone (truncated-cone-vec 1.2382 2.2382 #[0 0 2] #[0 0 2.1]))
         (speaker (union sound-lines
                         base
                         (difference outer-cone inner-cone)))
         (r (region3 '((-10 . 10) (-10 . 10) (-10 . 10))))
         (mesh (shape-to-mesh speaker r 20)))
    mesh))

(defun main-thread ()
  (tovero-initialize)
  (format t "~% *** Creating model, please wait...")
  (let* ((mesh (make-kinetophone-mesh))
         (color-vec (vector (/ 224 255) (/ 224 255) (/ 190 255)))
         (model (clive:separator
                 ((clive:material :ambient-color
                                  (clive:color :v (map 'vector (lambda (x) (* 0.1 x))
                                                       color-vec))
                                  :diffuse-color (clive:color :v color-vec))
                  (clive:vertex-property
                   :vertex (clive:foreign-vertex-array
                            (get-vertex-count mesh)
                            (get-vertices mesh)))
                  (clive:indexed-face-set
                   :coord-index (clive:foreign-coord-index-array
                                 (get-coord-index-count mesh)
                                 (get-coord-indices mesh)))))))
    (format t "model created. ***~%")
    (clive-viewer:set-model model)
    (clive-viewer:setup-main-loop :app-name "Kinetophone"
                                  :window-title "Kinetophone")
    ;; run viewer's loop
    (clive-viewer:run-main-loop)))

(defun main () (clive-viewer:run-thread #'main-thread '()))
