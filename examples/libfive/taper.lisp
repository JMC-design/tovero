#| taper.lisp

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

From the libfive source file: ./studio/examples/taper.io
Original license: GPL 2+

|#

(require 'tovero)
(tovero:use-tovero-package)

(defun taper-z (shape zmin zmax)
  (flet ((scale (z) (/ (- z zmax) (- zmax zmin))))
    (remap-shape (shape x y z)
                 (/ x (scale z))
                 (/ y (scale z))
                 z)))

(defun main ()
  (tovero-initialize)
  (let* ((taper (taper-z (sphere 1) -1 1.1))
         (bounds '((-10 . 10) (-10 . 10) (-10 . 10)))
         (resolution 60))
    (shape-to-mesh taper bounds resolution :file-name "taper.stl")
    (view-mesh "taper.stl")))
