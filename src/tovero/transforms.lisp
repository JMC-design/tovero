#| transforms.lisp

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

From the libfive source file: ./libfive/bind/transforms.scm
Original license: GPL 2+

|#

(in-package #:tovero)

;;;
;;; Affine
;;;

;;; Translation
(defun move (shape delta)
  "move shape #[dx dy [dz]]
  Moves the given shape in 2D or 3D space"
  (remap-shape (shape x y z)
               (- x (.x delta))
               (- y (.y delta))
               (- z (handler-case (.z delta)
                       (condition () 0)))))
(export 'move)
(declaim (inline translate))
(defun translate (shape delta)
  (move shape delta))
(export 'translate)

;;; Reflections

(defun reflect-x (shape &optional (x0 0))
  "reflect-x shape [x0]
  Reflect the given shape about the x origin or an optional offset"
  (remap-shape (shape x y z) (- (* 2 x0) x) y z))
(export 'reflect-x)

(defun reflect-y (shape &optional (y0 0))
  "reflect-y shape [y0]
  Reflect the given shape about the y origin or an optional offset"
  (remap-shape (shape x y z) x (- (* 2 y0) y) z))
(export 'reflect-y)

(defun reflect-z (shape &optional (z0 0))
  "reflect-z shape [z0]
  Reflect the given shape about the z origin or an optional offset"
  (remap-shape  (shape x y z) x y (- (* 2 z0) z)))
(export 'reflect-z)

(defun reflect-xy (shape)
  "reflect-xy shape
  Moves the given shape across the plane Y=X"
  (remap-shape (shape x y z) y x z))
(export 'reflect-xy)

(defun reflect-yz (shape)
  "reflect-xy shape
  Moves the given shape across the plane Y=Z"
  (remap-shape (shape x y z) x z y))
(export 'reflect-yz)

(defun reflect-xz (shape)
  "reflect-xx shape
  Moves the given shape across the plane X=Z"
  (remap-shape (shape x y z) z y x))
(export 'reflect-xz)


;;; Scaling

(defun scale-x (shape sx &optional (x0 0))
  "scale-x shape sx [x0]
  Scales a shape by sx on the x axis about 0 or an optional offset"
  (remap-shape (shape x y z) (+ x0 (/ (- x x0) sx)) y z))
(export 'scale-x)

(defun scale-y (shape sy &optional (y0 0))
  "scale-y shape sy [y0]
  Scales a shape by sy on the y axis about 0 or an optional offset"
  (remap-shape (shape x y z) x (+ y0 (/ (- y y0) sy)) z))
(export 'scale-y)

(defun scale-z (shape sz &optional (z0 0))
  "scale-z shape sz [z0]
  Scales a shape by sz on the z axis about 0 or an optional offset"
  (remap-shape (shape x y z) x y (+ z0 (/ (- z z0) sz))))
(export 'scale-z)

(defun scale-xyz (shape s &optional (center #[0 0 0]))
  "scale-xyz shape #[sx sy sz] [#[x0 y0 z0]]
  Scales a shape on all three axes, about 0 or an optional offset"
  (remap-shape (shape x y z)
               (+ (.x center) (/ (- x (.x center)) (.x s)))
               (+ (.y center) (/ (- y (.y center)) (.y s)))
               (+ (.z center) (/ (- z (.z center)) (.z s)))))
(export 'scale-xyz)

;;; Rotation

(defun rotate-x (shape angle &optional (center #[0 0 0]))
  "rotate-x shape angle [#[x0 y0 z0]]
  Rotate the given shape by an angle in radians
  The center of rotation is #[0 0 0] or specified by the optional argument"
  (let ((centered (move shape (- center)))
        (ca (cos angle))
        (sa (sin angle)))
    (move (remap-shape (centered x y z)
                       x
                       (+ (* ca y) (* sa z))
                       (+ (* (- sa) y) (* ca z))) center)))
(export 'rotate-x)

(defun rotate-y (shape angle &optional (center #[0 0 0]))
  "rotate-y shape angle [#[x0 y0 z0]]
  Rotate the given shape by an angle in radians
  The center of rotation is #[0 0 0] or specified by the optional argument"
  (let ((centered (move shape (- center)))
        (ca (cos angle))
        (sa (sin angle)))
    (move (remap-shape (centered x y z)
                       (+ (* ca x) (* sa z))
                       y
                       (+ (* (- sa) x) (* ca z))) center)))
(export 'rotate-y)

(defun rotate-z (shape angle &optional (center #[0 0 0]))
  "rotate-z shape angle [#[x0 y0 z0]]
  Rotate the given shape by an angle in radians
  The center of rotation is #[0 0 0] or specified by the optional argument"
  (let ((centered (move shape (- center)))
        (ca (cos angle))
        (sa (sin angle)))
    (move (remap-shape (centered x y z)
                       (+ (* ca x) (* sa y))
                       (+ (* (- sa) x) (* ca y))
                       z) center)))
(export 'rotate-z)
(declaim (inline rotate))
(defun rotate (shape angle &optional (center #[0 0 0]))
  (rotate-z shape angle center))
(export 'rotate)

;;;
;;; Non-affine
;;;

;;; Distortions

(defun taper-x-y (shape base h scale &optional (base-scale 1))
  "taper-x-y shape #[x0 y0] height scale [base-scale]
  Tapers a shape along the x axis as a function of y
  width = base-scale at base
  width = scale at base + #[0 h]"
  (move (remap-shape ((move shape (- base)) x y z)
    (* x (/ h (+ (* scale y) (* base-scale (- h y)))))
    y z) base))
(export 'taper-x-y)

(defun taper-xy-z (shape base h scale &optional (base-scale 1))
  "taper-xy-z shape #[x0 y0 z0] height scale [base-scale]
  Tapers a shape in the xy plane as a function of z
  width = base-scale at base
  width = scale at base + #[0 0 h]"
  (flet ((s (z)
           (/ h (+ (* scale z) (* base-scale (- h z))))))
    (move (remap-shape ((move shape (- base)) x y z)
                       (* x (s z))
                       (* y (s z))
                       z) base)))
(export 'taper-xy-z)

(defun shear-x-y (shape base h offset &optional (base-offset 0))
  "shear-x-y shape #[x0 y0] height offset [base-offset]
  Shears a shape on the x axis as a function of y
  offset = base-offset at base.y
  offset = offset = base.y + h"
  (remap-shape (shape x y z)
    (let ((f (/ (- y (.y base)) h)))
      (- x (* base-offset (- 1 f)) (* offset f)))
    y z))
(export 'shear-x-y)

(defun revolve-y (shape &optional (x0 0))
  "revolve-y shape [x0]
  Revolves a 2D (xy) shape around the vertical line x=x0"
  (union
   (remap-shape (shape x y z)
     (sqrt (+ (square x) (square z))) y z)
   (remap-shape (shape x y z)
     (- (sqrt (+ (square x) (square z)))) y z)))
(export 'revolve-y)

