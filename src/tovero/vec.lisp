#| vec.lisp

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

From the libfive source file: ./libfive/bind/vec.scm
Original license: GPL 2+

|#

(in-package #:tovero)

;;; protocol
(defgeneric add (a b))
(export 'add)
(defgeneric mul (a b))
(export 'mul)
(defgeneric sub (a b))
(export 'sub)
(defgeneric negate (a))
(export 'negate)
(defgeneric div (a b))
(export 'div)
(defgeneric norm (a))
(export 'norm)
(defgeneric dot (a b))
(export 'dot)

;;;
;;; vec2 class
;;;

(defclass vec2 ()
  ((x :reader .x :initarg :x)
   (y :reader .y :initarg :y)))

(defgeneric vec2 (x y))
(export 'vec2)
(defmethod vec2 ((x number) (y number))
  (make-instance 'vec2 :x x :y y))
(defmethod vec2 ((x shape) (y shape))
  (make-instance 'vec2 :x x :y y))
(export 'vec2)

(defmethod print-object ((self vec2) stream)
  (format stream "#[~a ~a]" (.x self) (.y self)))

(defmethod make-load-form ((self vec2) &optional env)
  (declare (ignore env))
  `(vec2 ',(.x self) ',(.y self)))

(defmethod add ((a vec2) (b vec2))
  (vec2 (add (.x a) (.x b)) (add (.y a) (.y b))))
(defmethod add ((a vec2) (b number))
  (vec2 (add (.x a) b) (add (.y a) b)))
(defmethod add ((a number) (b vec2)) (add b a))

(defmethod mul ((a vec2) (b number))
  (vec2 (mul (.x a) b) (mul (.y a) b)))
(defmethod mul ((a number) (b vec2)) (mul b a))

(defmethod sub ((a vec2) (b vec2))
  (vec2 (sub (.x a) (.x b)) (sub (.y a) (.y b))))
(defmethod sub ((a vec2) (b number))
  (vec2 (sub (.x a) b) (sub (.y a) b)))
(defmethod negate ((a vec2))
  (vec2 (negate (.x a)) (negate (.y a))))

(defmethod div ((a vec2) (b number))
  (vec2 (/ (.x a) b)
        (/ (.y a) b)))

(defmethod norm ((a vec2))
  (sqrt (+ (expt (.x a) 2)
           (expt (.y a) 2))))
(export 'norm)

(defmethod dot ((a vec2) (b vec2))
  (+ (* (.x a) (.x b))
     (* (.y a) (.y b))))
(export 'dot)

;;;
;;; vec3 class
;;;

;;; Note: 'vec3' class is defined in 'kernel.lisp'

(defmethod print-object ((self vec3) stream)
  (format stream "#[~a ~a ~a]" (.x self) (.y self) (.z self)))

(defmethod make-load-form ((self vec3) &optional env)
  (declare (ignore env))
  `(vec3 ',(.x self) ',(.y self) ',(.z self)))

(defgeneric vec3-by-3 (x y z))
(defmethod vec3-by-3 ((x number)
                      (y number)
                      (z number))
  (make-instance 'vec3 :x x :y y :z z))
(defmethod vec3-by-3 ((x shape)
                      (y shape)
                      (z shape))
  (make-instance 'vec3 :x x :y y :z z))
(defgeneric vec3-by-2 (a b))
(defmethod vec3-by-2 ((a vec2)
                      (b number))
  (let ((v (make-instance 'vec3)))
    (setf (slot-value v 'x) (.x a))
    (setf (slot-value v 'y) (.y a))
    (setf (slot-value v 'z) b)
    v))

(defun vec3 (a b &optional (c nil c-provided-p))
  (if c-provided-p
      (vec3-by-3 a b c)
      (vec3-by-2 a b)))
(export 'vec3)

(defmethod add ((a vec3) (b vec3))
  (vec3 (add (.x a) (.x b))
        (add (.y a) (.y b))
        (add (.z a) (.z b))))
(defmethod add ((a vec3) (b number))
  (vec3 (add (.x a) b)
        (add (.y a) b)
        (add (.z a) b)))
(defmethod add ((a number) (b vec3)) (add b a))

(defmethod mul ((a vec3) (b number))
  (vec3 (mul (.x a) b)
        (mul (.y a) b)
        (mul (.z a) b)))
(defmethod mul ((a number) (b vec3)) (mul b a))

(defmethod sub ((a vec3) (b vec3))
  (vec3 (sub (.x a) (.x b))
        (sub (.y a) (.y b))
        (sub (.z a) (.z b))))
(defmethod sub ((a vec3) (b number))
  (vec3 (sub (.x a) b)
        (sub (.y a) b)
        (sub (.z a) b)))
(defmethod negate ((a vec3))
  (vec3 (negate (.x a))
        (negate (.y a))
        (negate (.z a))))

(defmethod div ((a vec3) (b number))
  (vec3 (div (.x a) b)
        (div (.y a) b)
        (div (.z a) b)))

(defgeneric cross (a b))
(export 'cross)
(defmethod cross ((a vec3) (b vec3))
  (vec3 (sub (mul (.y a) (.z b)) (mul (.z a) (.y b)))
        (sub (mul (.z a) (.x b)) (mul (.x a) (.z b)))
        (sub (mul (.x a) (.y b)) (mul (.y a) (.x b)))))

(defmethod norm ((a vec3))
  (sqrt (+ (expt (.x a) 2)
           (expt (.y a) 2)
           (expt (.z a) 2))))

(defmethod dot ((a vec3) (b vec3))
  (+ (* (.x a) (.x b))
     (* (.y a) (.y b))
     (* (.z a) (.z b))))


;;;
;;; Create a reader macro to recognize #[a b [c]] syntax for creating
;;; vec2 and vec3 instances.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\[
   #'(lambda (stream char arg)
       (declare (ignore char arg))
       (let* ((elements (read-delimited-list #\] stream))
              (list-length (length elements)))
         (case list-length
           (0 nil)
           (2 `(vec2 ,(car elements)
                     ,(cadr elements)))
           (3 `(vec3 ,(car elements)
                     ,(cadr elements)
                     ,(caddr elements)))
           (otherwise
            (error
             "Wrong number of args for #[a b [c]]: ~a, should be 2 or 3."
             list-length))))))
  (set-macro-character #\] (get-macro-character #\) nil)))
