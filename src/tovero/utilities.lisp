#|
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

(in-package #:tovero)

(defparameter *mesh-viewer* "meshlab")
(export '*mesh-viewer*)

(defun view-mesh (mesh-file-name)
  #+SBCL(sb-ext:run-program *mesh-viewer* `(,mesh-file-name)
                            :search t :wait nil)
  #+ECL (ext:run-program *mesh-viewer*`(,mesh-file-name)
                         :wait nil :input nil :output nil)
  #-(or SBCL ECL) (signal 'wrong-type-arg
                          (concatenate
                           'string
                           "Mesh viewing not supported for this Common Lisp"
                           "distribution.")))
(export 'view-mesh)

(defmacro use-tovero-package ()
  `(progn
     (shadowing-import tovero:*arithmetic-functions*)
     (shadowing-import tovero:*csg-functions*)
     (use-package :tovero)))
(export 'use-tovero-package)

