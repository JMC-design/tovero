#| extra-vec.lisp

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

(defgeneric normalize (a))
(export 'normalize)

;;;
;;; for vec2 class
;;;

(defmethod normalize ((a vec2))
  "normalize #[ax ay]
  Normalize vector a, returning a unit vector and length."
  (let ((length (sqrt (+ (expt (.x a) 2)
                         (expt (.y a) 2)))))
    (values (/ a length) length)))

;;;
;;; for vec3 class
;;;

(defmethod normalize ((a vec3))
  "normalize #[ax ay az]
  Normalize vector a, returning a unit vector and length."
  (let ((length (sqrt (+ (expt (.x a) 2)
                         (expt (.y a) 2)
                         (expt (.z a) 2)))))
    (values (/ a length) length)))

(define-constant +x-axis-2+ #[1 0])
(export '+x-axis-2+)
(define-constant +y-axis-2+ #[0 1])
(export '+y-axis-2+)
(define-constant +origin-2+ #[0 0])
(export '+origin-2+)

(define-constant +x-axis+ #[1 0 0])
(export '+x-axis+)
(define-constant +y-axis+ #[0 1 0])
(export '+y-axis+)
(define-constant +z-axis+ #[0 0 1])
(export '+z-axis+)
(define-constant +origin+ #[0 0 0])
(export '+origin+)
