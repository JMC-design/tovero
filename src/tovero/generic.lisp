#| generic.lisp

Copyright 2018 Kavalogic, Inc.
Copyright (C) 2017  Matt Keeter

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

From the libfive source file: ./libfive/bind/libfive-guile.cpp
Original license: GPL 2+

|#

(in-package #:tovero)

;;;
;;; Setup "generic arithmetic", integrating Lisp numbers and
;;; libfive shapes (trees).
;;;

;;;   one argument functions
(defmacro overload1 (fun op shape-op)
  (let ((cl-fun (find-symbol (string-upcase (string `,fun))
                             "COMMON-LISP")))
    `(progn
       (defgeneric ,op (a))
       (defmethod ,op ((a number)) (,cl-fun a))
       (defmethod ,op ((a shape)) (shape (quote ,shape-op) a))
       (defun ,fun (a) (,op a))
       (export ',fun))))

(overload1 sqrt sqrt-op sqrt)
(overload1 sin sin-op sin)
(overload1 cos cos-op cos)
(overload1 tan tan-op tan)
(overload1 asin asin-op asin)
(overload1 acos acos-op acos)

(defgeneric constant (a))
(export 'constant)
(defmethod constant ((a shape)) (shape 'const-var a))

(defgeneric square-op (a))
(defmethod square-op ((a number)) (cl:* a a))
(defmethod square-op ((a shape)) (shape 'square a))
(defun square (a) (square-op a))
(export 'square)

(defgeneric reciprocal (a))
(export 'reciprocal)
(defmethod reciprocal ((a shape)) (shape 'recip a))
(defmethod reciprocal ((a number)) (cl:/ a))

(defgeneric negate (a))
(export 'negate)
(defmethod negate ((a shape)) (shape 'neg a))
(defmethod negate ((a number)) (cl:- a))

;;;   two argument functions
(defmacro overload2 (fun op shape-op &key single-value)
  (let ((single
         (if (null single-value) '(car args) `,single-value))
        (cl-fun (find-symbol (string-upcase (string `,fun))
                             "COMMON-LISP")))
    `(progn
       (defgeneric ,op (a b))
       (defmethod ,op ((a number) (b number)) (,cl-fun a b))
       (defmethod ,op ((a shape) (b shape)) (shape (quote ,shape-op) a b))
       (defmethod ,op ((a number) (b shape))
         (shape (quote ,shape-op) (ensure-shape a) b))
       (defmethod ,op ((a shape) (b number))
         (shape (quote ,shape-op) a (ensure-shape b)))
       (defun ,fun (&rest args)
         (if (= (length args) 1)
             ,single
             (reduce (symbol-function (quote ,op))
                     (cdr args) :initial-value (car args))))
       (export ',fun))))

(overload2 + add add)
(overload2 * mul mul)
(overload2 - sub sub :single-value (negate (car args)))
(overload2 / div div :single-value (reciprocal (car args)))
(overload2 min min-op min)
(overload2 max max-op max)
(overload2 mod mod-op mod)
(overload2 atan atan-op atan2)

(defgeneric expt (a b))
(export 'expt)
(defmethod expt ((a number) (b number)) (cl:expt a b))
(defmethod expt ((a shape) (b rational))
  (shape 'nth-root
        (shape 'pow a (numerator b))
        (denominator b)))

(defparameter *arithmetic-functions*
  '(;; one argument
    tovero::sqrt
    tovero::sin tovero::cos tovero::tan
    tovero::asin tovero::acos
    tovero::constant tovero::square
    tovero::reciprocal tovero::negate
    ;; two argument
    tovero::+ tovero::* tovero::- tovero::/
    tovero::min tovero::max tovero::mod
    tovero::atan
    tovero::expt))
(export '*arithmetic-functions*)

