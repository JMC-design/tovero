#| csg.lisp

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

From the libfive source file: ./libfive/bind/csg.scm
Original license: GPL 2+

|#

(in-package #:tovero)

(defun union (&rest args)
  "union a [b [c [...]]]
  Returns the union of any number of shapes"
  (apply #'min args))
(export 'union)

(defun intersection (&rest args)
  "intersection a [b [c [...]]]
  Returns the intersection of any number of shapes"
  (apply #'max args))
(export 'intersection)

(defun difference (a &rest bs)
  "difference a b [c [d [...]]]
  Subtracts any number of shapes from the first argument"
  (intersection a (inverse (apply #'union bs))))
(export 'difference)

(defun inverse (a)
  "inverse a
  Returns a shape that's the inverse of the input shape"
  (- a))
(export 'inverse)

(defun offset (s o)
  "offset shape o
  Expand or contract a given shape by an offset"
  (+ s o))
(export 'offset)

(defun clearance (a b o)
  "clearance a b o
  Expands shape b by the given offset then subtracts it from shape a"
  (difference a (offset b o)))
(export 'clearance)

(defun shell (shape o)
  "shell shape o
  Returns a shell of a shape with the given offset"
  (clearance shape shape o))
(export 'shell)

(defun blend (a b m)
  "blend a b m
  Blends two shapes by the given amount"
  (union a b (- (+ (sqrt (abs a)) (sqrt (abs b))) m)))
(export 'blend)

(defun morph (a b m)
  "morph a b m
  Morphs between two shapes.
  m = 0 produces a, m = 1 produces b"
    (+ (* a (- 1 m)) (* b m)))
(export 'morph)

(defun loft (a b zmin zmax)
  "loft a b zmin zmax
  Produces a blended loft between a (at zmin) and b (at zmax)
  a and b should be 2D shapes (i.e. invariant along the z axis) "
  (lambda-shape (x y z)
    (max (- z zmax) (- zmin z)
      (/ (+ (* (- z zmin) b) (* (- zmax z) a))
         (- zmax zmin)))))
(export 'loft)

(defparameter *csg-functions*
  '(tovero::union tovero::intersection tovero::difference
    tovero::inverse
    tovero::offset tovero::clearance tovero::shell
    tovero::blend tovero::morph tovero::loft))
(export '*csg-functions*)
