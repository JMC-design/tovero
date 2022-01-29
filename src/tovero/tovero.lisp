#| tovero.lisp

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

 ;;;                                 ;;;
;;; A Common Lisp binding for libfive ;;;
 ;;;                                 ;;;

(require 'cffi)
(require 'trivial-garbage)

(defpackage #:tovero
  (:use #:cl
        #:cffi
        #:trivial-garbage)
  (:shadow "SQRT" "SIN" "COS" "TAN" "ASIN" "ACOS"
           "+" "*" "MIN" "MAX" "-" "/" "MOD" "ATAN" "EXPT"
           "UNION" "INTERSECTION"))

(in-package #:tovero)

;;; Fix as described in the SBCL 1.3.21 User Manual for prevention
;;; of redefining constants
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
