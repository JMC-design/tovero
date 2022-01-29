#| addons.lisp

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

From from the Alexandria project file: ./numbers.lisp
Original license: Public Domain

|#

;;; Miscellaneous helper code

(in-package #:tovero)

;;; Note: if we use enough of this code consider adding
;;; the alexandria library to the project dependencies
;;; rather than duplicating here.

(declaim (inline iota))
(defun iota (n &key (start 0) (step 1))
  "Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1.

Examples:

  (iota 4)                      => (0 1 2 3)
  (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
  (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)
"
  (declare (type (integer 0) n) (number start step))
  (loop repeat n
        ;; KLUDGE: get numeric contagion right for the first element too
        for i = (cl:+ (cl:- (cl:+ start step) step)) then (cl:+ i step)
        collect i))


