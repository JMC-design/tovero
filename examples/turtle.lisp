#| turtle.lisp

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
  (clive:define-scene-type material)
  (clive:define-scene-type indexed-face-set)
  (clive:define-scene-type vertex-property)
  (clive:define-scene-type draw-style))

(defconstant +P+ 1.6180339889)

(defun icosohedron (&optional (P +P+))
  (let* ((-P (- P))
         (2P (* 2 P))
         (-2P (- 2P))
         (-1-2P (- -1 2P))
         (1+2P (+ 1 2P))
         (2+P (+ 2 P))
         (-2-P (- -2 P))
         (3P (* 3 P))
         (-3P (- 3P))
         (vertices
          (make-array
           60
           :initial-contents
           ;; TODO: use permutations to generate these
           (list
            #[ -P        2       -1-2P   ]
            #[ -1       -3P       0      ]
            #[  2+P     -2P       1      ]
            #[  0        1        3P     ]
            #[ -1       -2-P      2P     ]
            #[ -3P       0        1      ]
            #[  1       -2-P      2P     ]
            #[ -2P       1       -2-P    ]
            #[  1        2+P     -2P     ]
            #[  2        1+2P    -P      ]
            #[ -2-P     -2P       1      ]
            #[ -2-P      2P      -1      ]
            #[ -1       -2-P     -2P     ]
            #[ -P       -2       -1-2P   ]
            #[ -1-2P    -P        2      ]
            #[  1+2P     P        2      ]
            #[ -1        3P       0      ]
            #[  1+2P     P       -2      ]
            #[ -2        1+2P     P      ]
            #[ -1-2P    -P       -2      ]
            #[  3P       0        1      ]
            #[  P       -2        1+2P   ]
            #[  1+2P    -P       -2      ]
            #[  0       -1       -3P     ]
            #[  0       -1        3P     ]
            #[  1        2+P      2P     ]
            #[ -1-2P     P       -2      ]
            #[ -2        1+2P    -P      ]
            #[  1       -3P       0      ]
            #[  1+2P    -P        2      ]
            #[ -2P      -1       -2-P    ]
            #[  2P       1        2+P    ]
            #[  P        2        1+2P   ]
            #[  2+P      2P      -1      ]
            #[ -2-P     -2P      -1      ]
            #[  P       -2       -1-2P   ]
            #[  2       -1-2P     P      ]
            #[  3P       0       -1      ]
            #[ -2P       1        2+P    ]
            #[ -1-2P     P        2      ]
            #[ -P       -2        1+2P   ]
            #[  2+P     -2P      -1      ]
            #[ -1        2+P      2P     ]
            #[ -P        2        1+2P   ]
            #[  1        3P       0      ]
            #[ -2P      -1        2+P    ]
            #[  P        2       -1-2P   ]
            #[  1       -2-P     -2P     ]
            #[  2P       1       -2-P    ]
            #[  2P      -1        2+P    ]
            #[ -1        2+P     -2P     ]
            #[  2        1+2P     P      ]
            #[  2+P      2P       1      ]
            #[ -2       -1-2P    -P      ]
            #[  0        1       -3P     ]
            #[ -3P       0       -1      ]
            #[  2P      -1       -2-P    ]
            #[  2       -1-2P    -P      ]
            #[ -2       -1-2P     P      ]
            #[ -2-P      2P       1      ])))
         (pentagons
          '(#(12 47 35 23 13)  ; flipped
            #(18 59 11 27 16)
            #(19 30 7 26 55)
            #(6 4 40 24 21)    ; flipped
            #(42 25 32 3 43)   ; flipped
            #(44 9 33 52 51)
            #(8 50 0 54 46)    ; flipped
            #(56 22 37 17 48)  ; flipped
            #(49 31 15 20 29)
            #(5 39 38 45 14)
            #(34 10 58 1 53)   ; flipped
            #(57 28 36 2 41)))
         (hexagons
          '(#(10 14 45 40 4 58)
            #(11 26 7 0 50 27)
            #(42 43 38 39 59 18) ; flipped
            #(2 36 6 21 49 29)
            #(20 37 22 41 2 29)
            #(25 51 52 15 31 32)
            #(26 11 59 39 5 55)
            #(57 47 12 53 1 28)  ; flipped
            #(28 1 58 4 6 36)
            #(49 21 24 3 32 31)  ; flipped
            #(53 12 13 30 19 34) ; flipped
            #(20 15 52 33 17 37) ; flipped
            #(41 22 56 35 47 57)
            #(16 27 50 8 9 44)   ; flipped
            #(45 38 43 3 24 40)
            #(48 46 54 23 35 56)
            #(14 10 34 19 55 5)  ; flipped
            #(51 25 42 18 16 44)
            #(30 13 23 54 0 7)   ; flipped
            #(9 8 46 48 17 33))))

    ;; intersect all half-spaces from a triangle of each of the pentagons
    ;; and hexagons
    (apply #'intersection
           (append (map 'list
                        #'(lambda (pentagon)
                            (half-space-pts (aref vertices (aref pentagon 0))
                                            (aref vertices (aref pentagon 1))
                                            (aref vertices (aref pentagon 2))))
                        pentagons)
                   (map 'list
                        #'(lambda (hexagon)
                            (half-space-pts (aref vertices (aref hexagon 0))
                                            (aref vertices (aref hexagon 1))
                                            (aref vertices (aref hexagon 2))))
                        hexagons)))))

(defun my-rounded-cube (len r)
  ;; cube in #[-len/2 -len/2 0] to #[len/2 len/2 len]
  ;; with all edges and corners rounded at radius r
  (let* ((offset (/ len 2))
         (base (extrude-z
                (rounded-rectangle #[(- offset) (- offset)] #[offset offset] r)
                0
                len))
         (faceted (intersection
                   base
                   (move (rotate-x base (/ pi 2)) #[0 offset offset])
                   (move (rotate-y base (/ pi 2)) #[offset 0 offset])))
         (sph (difference
               (sphere r)
               (box #[(- r) 0 (- r)] #[r r r])
               (box #[(- r) (- r) 0] #[r r r])
               (box #[0 (- r) (- r)] #[r r r])))
         (corner (difference (box #[(- r) (- r) (- r)] #[ 0 0 0]) sph))
         (half-corners
          (union
           (move corner #[(- r offset) (- r offset) r])
           (move (rotate-y corner (/ pi 2)) #[(- offset r) (- r offset) r])
           (move (rotate-x corner (/ pi 2)) #[(- r offset) (- offset r) r])
           (move
            (rotate-y (rotate-x corner (/ pi 2)) (/ pi 2))
            #[(- offset r) (- offset r) r])))
         (corners
          (union
           half-corners
           (move (rotate-x half-corners pi) #[0 0 len]))))
    (difference faceted corners)))

(defun make-turtle-mesh ()
  (let* ((r (region3 '((-10 . 10) (-10 . 10) (-10 . 10))))
         (bball-radius (sqrt (+ (* 9 (* +P+ +P+)) 1)))
         (base-sphere (sphere (* bball-radius 0.925)))
         ;; bottom
         (bottom (scale base-sphere #[1 0.225 1]))
         ;; head
         (head-cutout-sphere (scale (move (sphere 2)
                                          #[(+ bball-radius 0) 1.5 0])
                                    #[1 .5 1]))
         ;; shell
         (shell-sphere (sphere bball-radius))
         (bball (icosohedron))
         (shell (morph bball shell-sphere .3))
         (half-shell (difference shell (half-space #[0 1 0])))
         (base-shell-unscaled (union half-shell
                                     bottom))
         (base-shell (scale base-shell-unscaled #[1.25 1 1]))
         (shell-cutout (scale (sphere bball-radius) #[1 0.15 1.1]))
         ;; body
         (body (intersection
                (move (scale base-sphere #[1.1 0.5 1])
                      #[0 0.5 0])
                base-shell))
         ;; shell assembly
         (turtle-shell
          (difference base-shell
                      ;; head area
                      (move shell-cutout #[(+ bball-radius 0.75) 0.4 0])
                      head-cutout-sphere
                      ;; tail area
                      (move shell-cutout #[(- (+ bball-radius 0.75)) 0.4 0])))
         ;; head/neck
         (neck (cylinder-z 0.8 3.0 #[0 0.1 0]))
         (eye (sphere 0.35))
         ;; (mouth (ang-wedge (/ pi 50) 2 2))
         (head (union
                (move (scale (my-rounded-cube 2.2 0.8) #[1 1 1.1]) #[0 0.4 -1.3])
                (move eye #[0.77 0.6 -0.7])
                (move eye #[-0.77 0.6 -0.7])))
         (head/neck (scale (move
                            (rotate-z (rotate-y (union neck head) (/ pi 2))
                                      (/ pi 12))
                            #[6.75 1.05 0])
                           #[1 1 0.95]))
         (leg (rotate-z (scale (sphere 1.0) #[2.2 0.4 0.7]) (/ pi 9)))
         (legs (union
                (move (rotate-y leg (/ pi 5)) #[-3.5 0.5 -3.3])
                (move (rotate-y leg (- (/ pi 5))) #[-3.5 0.5 3.3])
                (move (rotate-y leg (* pi (/ 4 5))) #[3.5 0.5 -3.3])
                (move (rotate-y leg (- (* pi (/ 4 5)))) #[3.5 0.5 3.3])))
         (tail (move (scale (sphere 1.0) #[2.0 0.3 0.35]) #[-5 0.5 0]))
         ;; turtle assembly
         (turtle (union turtle-shell body head/neck legs tail))
         (shape-mesh (shape-to-mesh turtle r 10)))
    shape-mesh))

(defun main-thread ()
  (tovero-initialize)
  (format t "~% *** Creating model, please wait...")
  (let* ((mesh (make-turtle-mesh))
         (model (clive:separator
                 ((clive:material :ambient-color #(.01 .2 .01)
                                  :diffuse-color #(.2 .8 .2)
                                  :shininess 0.0)
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
    (clive-viewer:setup-main-loop :app-name "Ao" :window-title "Ao")
    ;; run viewer's loop
    (clive-viewer:run-main-loop)))

(defun main () (clive-viewer:run-thread #'main-thread '()))
