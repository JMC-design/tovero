#| extra-shapes.lisp

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

(defun half-space-pts (point1 point2 point3)
  "half-space-pts point1 point2 point3
  (specify points in CW order for RH outward facing normal)"
  (let* ((V1 (- point1 point2))
         (V2 (- point3 point2))
         (X (cross V1 V2))
         (l (norm X))
         (N (/ X l))
         (pos (/ (+ point1 point2 point3) 3)))
    (half-space N pos)))
(export 'half-space-pts)

(defun cylinder-vec (radius height-vec &optional (base-pt #[0 0 0]))
  "cylinder-vec radius #[hx hy hz] [#[x0 y0 z0]]
  A right cylinder with direction and length of 'height-vec', having
  an overall 'radius', optionally starting at 'base-pt'."
  (let* ((l (norm height-vec))
         ;; make a cylinder of the same dimensions aligned along
         ;; the z axis
         (z-cyl (cylinder-z radius l)))
    ;; rotate and translate it into position
    (move
     (rotate-rel z-cyl +z-axis+ height-vec)
     base-pt)))
(export 'cylinder-vec)

(defun torus-vec (normal outer-radius inner-radius
                  &optional (center #[0 0 0]))
  "torus-vec normal #[ux uy uz] outer-radius inner-radius [#[x0 y0 z0]]
  A torus facing in the direction of the 'normal' vector, with a major
  radius of 'outer-radius' and a minor radius of 'inner-radius',
  optionally centered at 'center'."
  (let* (;; construct a torus at the origin facing towards
         ;; the z axis
         (z-torus (torus-z outer-radius inner-radius)))
    ;; rotate and translate it into position
    (move
     (rotate-rel z-torus +z-axis+ normal)
     center)))
(export 'torus-vec)

(defun truncated-cone-vec (base-radius top-radius height-vec
                           &optional (base-pt #[0 0 0]))
  "truncated-cone-vec base-radius top-radius #[hx hy hz] [#[x0 y0 z0]]
  A right truncated cone with direction and length of 'height-vec'
  having 'base-radius' tapered to 'top-radius', optionally starting at
  'base-pt'."
  (let* ((l (norm height-vec))
         ;; construct a right cylinder of similar dimensions aligned
         ;; along the z-axis
         (z-cyl (cylinder-z base-radius l))
         (scale (/ top-radius base-radius)))
    ;; taper, rotate, and translate it into position
    (move
     (rotate-rel
      (taper-xy-z z-cyl #[0 0 0] l scale)
      +z-axis+
      height-vec)
     base-pt)))
(export 'truncated-cone-vec)

(defun box-dim (vertex length width height)
  "box-dim #[v1 v2 v3] #[lx ly lz] #[wx wy wz] #[hx hy hz]
  A (right) box with arbitrary orientation constructed by extending
  the edges from the 'vertex' point along the 'length', 'width', and
  'height' vectors."
  (multiple-value-bind (length-unit length-len)
      (normalize length)
    (multiple-value-bind (width-unit width-len)
        (normalize width)
      (multiple-value-bind (height-unit height-len)
          (normalize height)
        (let (;; construct an axis aligned box of the same dimensions
              ;; needed for the resultant box
              (axis-aligned-box (box #[0 0 0]
                                      #[length-len width-len height-len])))
          ;; rotate and translate it into position
          (move
           (rotate-basis axis-aligned-box
                         length-unit width-unit height-unit)
           vertex))))))
(export 'box-dim)

(defun wedge (vertex depth height width)
  "wedge #[v1 v2 v3] #[d1 d2 w3] #[h1 h2 h3] #[w1 w2 w3]
  Right angle wedge with arbitrary orientation constructed by extending
  the edges from the 'vertex' point along the 'depth', 'height', and
  'width' vectors, with the height tapering to 0 with increasing depth."
  (multiple-value-bind (depth-unit depth-len)
      (normalize depth)
    (multiple-value-bind (height-unit height-len)
        (normalize height)
      (multiple-value-bind (width-unit width-len)
          (normalize width)
        (let (;; construct an axis aligned wedge of the same dimensions
              ;; needed for the resultant wedge
              (axis-aligned-wedge
               (extrude-z
                (triangle #[0 0] #[depth-len 0] #[0 height-len])
                0 width-len)))
          ;; rotate and translate it into position
          (move
           (rotate-basis axis-aligned-wedge
                         depth-unit height-unit width-unit)
           vertex))))))
(export 'wedge)
