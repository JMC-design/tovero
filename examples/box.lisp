#| box.lisp

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

(require 'tovero)

(tovero:use-tovero-package)

(defun box-space (l w h)
  (let* ((l/2 (/ l 2))
         (w/2 (/ w 2))
         (h/2 (/ h 2)))
    (intersection
     (half-space #[ 0  1  0] #[      0     h/2       0])
     (half-space #[ 0 -1  0] #[      0 (- h/2)       0])
     (half-space #[ 1  0  0] #[    w/2       0       0])
     (half-space #[-1  0  0] #[(- w/2)       0       0])
     (half-space #[ 0  0  1] #[      0       0     l/2])
     (half-space #[ 0  0 -1] #[      0       0 (- l/2)]))))

(defun box-space-pts (l w h)
  (let* ((l/2 (/ l 2))
         (w/2 (/ w 2))
         (h/2 (/ h 2))
         (-l/2 (- l/2))
         (-w/2 (- w/2))
         (-h/2 (- h/2)))
    (intersection
     ;; TODO: use permutations to generate these
     (half-space-pts #[-w/2  h/2  l/2]
                     #[-w/2  h/2 -l/2]
                     #[ w/2  h/2 -l/2])
     (half-space-pts #[-w/2 -h/2 -l/2]
                     #[-w/2 -h/2  l/2]
                     #[ w/2 -h/2  l/2])
     (half-space-pts #[ w/2 -h/2  l/2]
                     #[ w/2  h/2  l/2]
                     #[ w/2  h/2 -l/2])
     (half-space-pts #[-w/2 -h/2 -l/2]
                     #[-w/2  h/2 -l/2]
                     #[-w/2  h/2  l/2])
     (half-space-pts #[-w/2 -h/2  l/2]
                     #[-w/2  h/2  l/2]
                     #[ w/2  h/2  l/2])
     (half-space-pts #[ w/2  h/2 -l/2]
                     #[-w/2  h/2 -l/2]
                     #[-w/2 -h/2 -l/2]))))

(let* ((b (box-space 1 1 1))
       (bounds (region3 '((-2 . 2) (-2 . 2) (-2 . 2))))
       (resolution 100))
    (shape-to-mesh b bounds resolution :file-name "box.stl"))

(let* ((b (box-space-pts 1 1 1))
       (na-hs (half-space #[1 1 1] #[0.1 0.1 0.1]))
       (box (intersection b na-hs))
       (bounds (region3 '((-2 . 2) (-2 . 2) (-2 . 2))))
       (resolution 100))
    (shape-to-mesh box bounds resolution :file-name "na-box.stl"))
