#| shapes.lisp

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

From the libfive source file: ./libfive/bind/shapes.scm
Original license: GPL 2+

|#

(in-package #:tovero)

(defun circle (r &optional (center #[0 0]))
  "circle r [#[x y]]
  A 2D circle with the given radius and optional center"
  (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y))) r))
        center))
(export 'circle)

(defun ring (ro ri &optional (center #[0 0]))
  "ring ro ri [#[x y]]
  A 2D ring with the given outer/inner radii and optional center"
  (difference (circle ro center) (circle ri center)))
(export 'ring)

(defun polygon (r n &optional (center #[0 0]))
  "polygon r n [#[x y]]
  A polygon with center-to-vertex distance r and n sides"
  (let* ((r (* r (cos (/ cl:pi n))))
         (half (lambda-shape (x y z) (- y r))))
    (move (apply #'intersection
                 (map 'list
                      (lambda (i) (rotate-z half (* 2 cl:pi i (/ n))))
                      (iota n)))
          center)))
(export 'polygon)

(defun rectangle (a b)
  "rectangle #[xmin ymin] #[xmax ymax]
  A rectangle with the given bounding corners"
  (lambda-shape (x y z)
    (max (- (.x a) x) (- x (.x b)) (- (.y a) y) (- y (.y b)))))
(export 'rectangle)

(defun rounded-rectangle (a b r)
  "rounded-rectangle #[xmin ymin] #[xmax ymax] r
  A rectangle with rounded corners"
  (union (rectangle (+ a (vec2 0 r)) (- b (vec2 0 r)))
         (rectangle (+ a (vec2 r 0)) (- b (vec2 r 0)))
         (circle r (+ a r))
         (circle r (- b r))
         (circle r (vec2 (+ (.x a) r) (- (.y b) r)))
         (circle r (vec2 (- (.x b) r) (+ (.y a) r)))))
(export 'rounded-rectangle)

(defun rectangle-centered-exact (b &optional (c #[0 0 0]))
  "rectangle-centered-exact #[width height]
  Constructs an exact-field rectangle at the origin"
  (move
   (lambda-shape (x y z)
     (let ((d (- #[(abs x) (abs y)] (/ b 2))))
       (+ (min (max (.x d) (.y d)) 0)
          (norm #[(max (.x d) 0) (max (.y d) 0)]))))
   c))
(export 'rectangle-centered-exact)

(defun rectangle-exact (a b)
  "rectangle-exact #[xmin ymin] #[xmax ymax]
  Constructs a rectangle from an exact distance field"
  (let ((size (- b a))
        (center (/ (+ a b) 2)))
    (rectangle-centered-exact size center)))
(export 'rectangle-exact)

(defun rounded-rectangle-exact (a b r)
  "rounded-rectangle-exact #[xmin ymin] #[xmax ymax] r
  A rectangle with rounded corners, with an exact distance field"
  (offset (rectangle-exact (+ a r) (- b r)) r))
(export 'rounded-rectangle-exact)

(defun triangle (a b c)
  "triangle #[x0 y0] #[x1 y1] #[x2 y2]
  A 2D triangle"
  (flet ((half-plane (a b)
         (lambda-shape (x y z)
           (- (* (- (.y b) (.y a)) (- x (.x a)))
              (* (- (.x b) (.x a)) (- y (.y a)))))))
    (if (< 0 (.z (cross (vec3 (- b a) 0) (vec3 (- c a) 0))))
        (intersection
         (half-plane a b) (half-plane b c) (half-plane c a))
        (intersection
         (half-plane a c) (half-plane c b) (half-plane b a)))))
(export 'triangle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun box (a b)
  "box #[xmin ymin zmin] #[xmax ymax zmax]
  A box with the given bounds"
  ;; since extrude-z is not defined (yet) in the source code
  #+SBCL (declare (sb-ext:muffle-conditions style-warning))
  (extrude-z (rectangle a b) (.z a) (.z b)))
(export 'box)

(defun box-centered (s &optional (m #[0 0 0]))
  "box-centered #[xsize ysize zsize] [#[x0 y0 z0]]
  A box with the given size, centered around the given point"
  (box (- m (/ s 2)) (+ m (/ s 2))))
(export 'box-centered)

(defun half-space (norm &optional (pos #[0 0 0]))
  "half-space #[nx ny nz] [#[px py pz]]
  A plane that divides the world into inside and outside
  Arguments are the plane normal and optional position"
  (lambda-shape (x y z)
    (dot (- #[x y z] pos) norm)))
(export 'half-space)

(defun sphere (r &optional (center #[0 0 0]))
  "sphere r [#[x y z]]
  A sphere with the given radius and optional center"
  (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y) (square z))) r))
        center))
(export 'sphere)

(defun cylinder-z (r h &optional (base #[0 0 0]))
  "cylinder-z r h [#[x0 y0 z0]]
  A cylinder (oriented along the Z axis)"
  ;; since extrude-z is not defined (yet)
  #+SBCL (declare (sb-ext:muffle-conditions style-warning))
  (extrude-z (circle r base) (.z base) (+ h (.z base))))
(export 'cylinder-z)
(declaim (inline cylinder))
(defun cylinder (r h &optional (base #[0 0 0])) (cylinder-z r h base))
(export 'cylinder)

(defun cone-z (r height &optional (base #[0 0 0]))
  "cone-z r height [#[x y z]]
  Creates a cone from a radius, height, and optional base location"
  (taper-xy-z (cylinder-z r height base) base height 0))
(export 'cone-z)
(declaim (inline cone))
(defun cone (r height &optional (base #[0 0 0])) (cone-z r height base))
(export 'cone)

(defun pyramid-z (a b zmin height)
  "pyramid-z #[xmin ymin] #[xmax ymax] zmin dz
  Creates a pyramid from a base rectangle, lower z value and height"
  ;; since extrude-z is not defined (yet)
  #+SBCL (declare (sb-ext:muffle-conditions style-warning))
  (taper-xy-z (extrude-z (rectangle a b) zmin (+ zmin height))
              (vec3 (/ (+ a b) 2) zmin) height 0))
(export 'pyramid-z)
(declaim (inline pyramid))
(defun pyramid (a b zmin height) (pyramid-z a b zmin height))
(export 'pyramid)

(defun torus-z (outer-radius inner-radius &optional (center #[0 0 0]))
  "torus-z R r [#[x y z]]
  Create a torus from the given outer radius, inner radius, and optional center"
  (flet ((c (a b) (sqrt (+ (square a) (square b)))))
    (move (lambda-shape (x y z) (- (c (- outer-radius (c x y)) z) inner-radius))
          center)))
(export 'torus-z)
(declaim (inline torus))
(defun torus (outer-radius inner-radius &optional (center #[0 0 0]))
  (torus-z outer-radius inner-radius center))
(export 'torus)

(defun gyroid (period thickness)
  "gyroid #[x-period y-period z-period] thickness
  Create a volume-filling gyroid with the given periods and thickness"
  (let* ((tau (* 2 cl:pi))
         (x-factor (/ tau (.x period)))
         (y-factor (/ tau (.y period)))
         (z-factor (/ tau (.z period))))
    (shell (lambda-shape (x y z)
             (+ (* (sin (/ x x-factor))
                   (cos (/ y y-factor)))
                (* (sin (/ y y-factor))
                   (cos (/ z z-factor)))
                (* (sin (/ z z-factor))
                   (cos (/ x x-factor)))))
           (- thickness))))
(export 'gyroid)

(defun rounded-box (a b r)
  "rounded-box #[xmin ymin zmin] #[xmax ymax zmax] r
  Rounded box with the given bounds and radius (as a 0-1 fraction)"
  (let* ((d (- b a))
         (r-prime (* r (min (.x d) (.y d) (.z d)) (/ 2)))
         (corners
          (apply #'union
                 (map 'list
                      (lambda (i)
                        (sphere r-prime
                                #[(if (logbitp 0 i)
                                      (+ (.x a) r-prime)
                                      (- (.x b) r-prime))
                                  (if (logbitp 1 i)
                                      (+ (.y a) r-prime)
                                      (- (.y b) r-prime))
                                  (if (logbitp 2 i)
                                      (+ (.z a) r-prime)
                                      (- (.z b) r-prime))]))
                      (iota 8)))))
    (flet (;; Constructs a set of 4 edges for the given lengths
           (edges (x y z)
             (apply #'union
                    (map 'list
                         (lambda (i)
                           (cylinder-z r-prime (- z (* 2 r-prime))
                                       #[(if (logbitp 0 i)
                                             r-prime
                                             (- x r-prime))
                                         (if (logbitp 1 i)
                                             r-prime
                                             (- y r-prime))
                                         r-prime]))
                         (iota 4)))))
         (let ((edges-x (reflect-xz (edges (.z d) (.y d) (.x d))))
               (edges-y (reflect-yz (edges (.x d) (.z d) (.y d))))
               (edges-z (edges (.x d) (.y d) (.z d))))
           (union
            corners
            (move (union edges-z edges-y edges-x) a)
            (extrude-z
             (union (rectangle #[(+ (.x a) r-prime) (.y a)]
                               #[(- (.x b) r-prime) (.y b)])
                    (rectangle #[(.x a) (+ (.y a) r-prime)]
                               #[(.x b) (- (.y b) r-prime)]))
             (+ (.z a) r-prime) (- (.z b) r-prime))
            (extrude-z (rectangle (+ a r-prime) (- b r-prime))
                       (.z a) (.z b)))))))
(export 'rounded-box)
(declaim (inline rounded-cube))
(defun rounded-cube (a b r) (rounded-box a b r))
(export 'rounded-cube)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher-order shapes

(defun array-x (shape i dx)
  "array-x shape i dx
  Iterates a part in a 1D array.
  i is the number of repetitions
  dx is the offset on the X axis"
  (apply #'union
         (map 'list (lambda (i) (move shape (* (vec2 dx 0) i))) (iota i))))
(export 'array-x)

(defun array-xy (shape i j dxy)
  "array-xy shape i j #[dx dy]
  Iterates a part in a 2D array.
  i and j are part counts along each axis
  dxy is a vec2 representing offsets along each axis"
  (let ((x (array-x shape i (.x dxy))))
    (apply #'union
           (map 'list (lambda (j) (move x (* (vec2 0 (.y dxy)) j))) (iota j)))))
(export 'array-xy)

(defun array-xyz (shape i j k dxyz)
  "array-xyz shape i j k #[dx dy dz]
  Iterates a part in a 2D array.
  i, j, k are part counts along each axis
  dxyz is a vec3 representing offsets along each axis"
  (let ((xy (array-xy shape i j dxyz)))
    (apply #'union
           (map 'list (lambda (k) (move xy (* (vec3 0 0 (.z dxyz)) k)))
                (iota k)))))
(export 'array-xyz)

(defun extrude-z (shape zmin zmax)
  "extrude-z shape zmin zmax
  Extrudes a 2D shape between za and zb"
  (max shape (lambda-shape (x y z) (max (- zmin z) (- z zmax)))))
(export 'extrude-z)
(declaim (inline extrude))
(defun extrude (shape zmin zmax) (extrude-z shape zmin zmax))
(export 'extrude)

(defun array-polar (shape n &optional (c #[0 0]))
  "array-polar shape n [#[x y]]
  Iterates a shape about an optional center position"
  (apply #'union
         (map 'list
              (lambda (i) (rotate-z shape (* 2 cl:pi (/ i n)) c))
              (iota n))))
(export 'array-polar)
