#| extra-transforms.lisp

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

(defun scale (shape s &optional offset)
  "scale shape #[sx sy sz] [ #[xo yo zo] ]
  General scaling for a shape with an optional offset."
  (flet ((scale-factors ()
           (if (subtypep (type-of s) 'vec3)
               (values (.x s) (.y s) (.z s))
               (values s s s))))
    (multiple-value-bind (sx sy sz)
        (scale-factors)
      (if (null offset)
          (remap-shape (shape x y z)
                       (/ x sx)
                       (/ y sy)
                       (/ z sz))
          (let ((x0 (.x offset))
                (y0 (.y offset))
                (z0 (handler-case (.z offset)
                      (condition () 0))))
            (remap-shape (shape x y z)
                         (+ x0 (/ (- x x0) sx))
                         (+ y0 (/ (- y y0) sy))
                         (+ z0 (/ (- z z0) sz))))))))
(export 'scale)

(defparameter *cosine-tol* 1.0E-3)
(defparameter *epsilon* 1.0E-6)
(defun float-equal-p (a b)
  (or
   (and (zerop a) (zerop b))
   (< (abs (if (or (zerop a) (zerop b))
               (- a b)
               (/ (- a b) a)))
      *epsilon*)))
(defun close-enuf-cosine-p (expected actual)
  (let ((*epsilon* *cosine-tol*))
    (float-equal-p expected actual)))

;;; return unit vector perpendicular to 'a' (standard vector approach)
(defun unit-perpendicular-e_i (a)
  ;; find the vector component i with the least magnitude for a to get
  ;; e_i (standard vector)
  (let* ((x (abs (.x a)))
         (y (abs (.y a)))
         (z (abs (.z a)))
         (b (if (and (<= x y) (<= x z))
                #[1 0 0] ; e_0
                (if (<= y z)
                    #[0 1 0] ; e_1
                    #[0 0 1])))) ; e_2
    (normalize (cross a b))))

;;; [Source for rotation matrix: https://en.wikipedia.org/wiki/Rotation_matrix]
;;; TODO: quaternion and vector based
(defun rotate-rel (shape vec-from vec-to &optional (center #[0 0 0]))
  "rotate-relative shape vec-from vec-to [#[0 0 0]]
  Reorient the given shape from its original direction (vec-from) to a
  new direction (vec-to). The center of rotation is #[0 0 0] or
  specified by the optional argument."
  (let* ((centered (move shape (- center)))
         (unit-from (normalize vec-from))
         (unit-to (normalize vec-to))
         (ca (dot unit-from unit-to))) ; dot product is the cosine
    (if (close-enuf-cosine-p 1.0 ca)
        ;; parallel, same dir:
        shape
        (let* ((u (if (close-enuf-cosine-p -1.0 ca)
                      ;; parallel, opposite dir:
                      (unit-perpendicular-e_i vec-from)
                      (normalize (cross unit-from unit-to))))
               (one-minus-ca (- 1 ca))
               (angle (acos ca))
               (sa (sin angle))
               (ux (.x u)) (uy (.y u)) (uz (.z u))
               (ux*uy (* ux uy)) (ux*uz (* ux uz)) (uy*uz (* uy uz))
               (ux*sa (* ux sa)) (uy*sa (* uy sa)) (uz*sa (* uz sa))
               (ux*uy*one-minus-ca (* ux*uy one-minus-ca))
               (ux*uz*one-minus-ca (* ux*uz one-minus-ca))
               (uy*uz*one-minus-ca (* uy*uz one-minus-ca)))
          (move (remap-shape (centered x y z)
                             ;; Rx
                       (+ (* (+ ca (* (square ux) one-minus-ca)) x)   ; R[0,0]
                          (* (- ux*uy*one-minus-ca uz*sa)        y)   ; R[0,1]
                          (* (+ ux*uz*one-minus-ca uy*sa)        z))  ; R[0,2]
                       ;; Ry
                       (+ (* (+ ux*uy*one-minus-ca uz*sa)        x)   ; R[1,0]
                          (* (+ ca (* (square uy) one-minus-ca)) y)   ; R[1,1]
                          (* (- uy*uz*one-minus-ca ux*sa)        z))  ; R[1,2]
                       ;; Rz
                       (+ (* (- ux*uz*one-minus-ca uy*sa)        x)   ; R[2,0]
                          (* (+ uy*uz*one-minus-ca ux*sa)        y)   ; R[2,1]
                          (* (+ ca (* (square uz) one-minus-ca)) z))) ; R[2,2]
                center)))))
(export 'rotate-relative)

(defun rotate-basis (shape basis-x basis-y basis-z)
  "rotate-basis shape #[bx-x bx-y bx-z] #[by-x by-y by-z] #[bz-x bz-y bz-z]
  Reorient the given shape to the new basis around the origin."
  ;; multiply the coordinates by the transpose of the new basis
  ;; since M^-1 <-> M^T for a rotation matrix
  (remap-shape (shape x y z)
               (+ (* (.x basis-x) x) (* (.y basis-x) y) (* (.z basis-x) z))
               (+ (* (.x basis-y) x) (* (.y basis-y) y) (* (.z basis-y) z))
               (+ (* (.x basis-z) x) (* (.y basis-z) y) (* (.z basis-z) z))))
(export 'rotate-basis)
