#| kernel.lisp

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

(define-foreign-library lib-tovero
    (:unix (:or "libTovero.so.0" "libTovero.so"))
  (t (:default "libTovero")))
(use-foreign-library lib-tovero)

(defmacro concat (&body body)
  `(concatenate 'string ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-to-symbol (string &optional package)
    (multiple-value-bind (symbol status)
        (intern (string-upcase string)
                (if (null package) "TOVERO" package))
      (values symbol status))))

(declaim (inline c-int-to-boolean))
(defun c-int-to-boolean (value)
  (not (= value 0)))
(declaim (inline boolean-to-c-int))
(defun boolean-to-c-int (value)
  (if (null value) 0 1))
(declaim (inline f-val))
(defun f-val (n) (coerce n 'single-float))

;;;
;;; Classes
;;;

;;;   c++-proxy class
;;;   This class wraps a foreign C++ object pointer.
(defclass c++-proxy ()
  ((c++-object :initform nil
               :initarg :c++-object
               :reader to-foreign)))
(defgeneric setup-object-finalizer (self destructor))
(defmethod setup-object-finalizer ((self c++-proxy)
                                   destructor)
  (let* ((pointer (to-foreign self)))
    (finalize self
              (lambda ()
                (funcall destructor pointer))))
  self)
(defgeneric destructor (self))
(defmethod destructor ((self c++-proxy))
  nil)
(defmethod initialize-instance :after ((self c++-proxy) &key managingp)
  (let ((pointer (to-foreign self)))
    (unless pointer
      (error "Underlying pointer for C++ object not set.")))
  (when managingp
    (let ((self-destructor (destructor self)))
      (if (null self-destructor)
          (error "No destructor found for class: '~a' (object: ~a)."
                 (class-name (class-of self))
                 self)
          (setup-object-finalizer self self-destructor)))))

;;;   'c++proxy' subclasses
(defclass contour (c++-proxy) ())
(export 'contour)
(defclass contours (c++-proxy) ())
(export 'contours)
(defclass mesh (c++-proxy) ())
(export 'mesh)
(defclass mesh-contiguous (mesh) ())
(export 'mesh-contiguous)
(defclass mesh-delimited (mesh) ())
(export 'mesh-delimited)
(defclass pixels (c++-proxy) ())
(export 'pixels)
(defclass voxel-slice (c++-proxy) ())
(export 'voxel-slice)
(defclass shape (c++-proxy)
  ((meta :reader get-shape-meta)))
(export 'shape)
(export 'get-shape-meta)
;;;   mirrored (not c++-proxy)
(defclass interval ()
  ((lower :reader get-lower :type number :initarg :lower)
   (upper :reader get-upper :type number :initarg :upper)))
(export 'get-lower)
(export 'get-upper)
(defgeneric interval-2 (lower upper))
(export 'interval-2)
(defclass region2 ()
  ((x :reader get-x :type number :initarg :x)
   (y :reader get-y :type number :initarg :y)))
(defgeneric region2-xy (x y))
(export 'region2-xy)
(defclass region3 ()
  ((x :reader get-x :type number :initarg :x)
   (y :reader get-y :type number :initarg :y)
   (z :reader get-z :type number :initarg :z)))
(defgeneric region3-xyz (x y z))
(export 'region3-xyz)

;;;
;;;   CFFI type setup
;;;

;;;   structs
;;;     tovero_interval
(defcstruct %tovero-interval-struct
  (lower :float)
  (upper :float))
;;;     tovero_region2
(defcstruct %tovero-region2-struct
  (x (:struct %tovero-interval-struct))
  (y (:struct %tovero-interval-struct)))
;;;     tovero_region3
(defcstruct %tovero-region3-struct
  (x (:struct %tovero-interval-struct))
  (y (:struct %tovero-interval-struct))
  (z (:struct %tovero-interval-struct)))
;;;     tovero_vec3
(defcstruct %tovero-vec3-struct
  (x :float)
  (y :float)
  (z :float))
;;;     tovero_contour
(defcstruct %tovero-contour-struct
  (points (:pointer (:struct %tovero-vec3-struct)))
  (count :uint32))
;;;     tovero_contours
(defcstruct %tovero-contours-struct
  (contours (:pointer (:struct %tovero-contour-struct)))
  (count :uint32))
;;;     tovero_mesh_contiguous
(defcstruct %tovero-mesh-contiguous-struct
  (vertices (:pointer :float))
  (triangle-indices (:pointer :uint32))
  (triangle-count :uint32)
  (vertex-count :uint32))
;;;     tovero_mesh_delimited
(defcstruct %tovero-mesh-delimited-struct
  (vertices (:pointer :float))
  (vertex-count :uint32)
  (coord-indices (:pointer :int32))
  (coord-index-count :uint32))

;;;     pixels
(defcstruct %tovero-pixels-struct
  (pixels (:pointer :bool))
  (width :uint32)
  (height :uint32))
;;;     voxel-slice
(defcstruct %tovero-voxel-slice-struct
  (voxels (:pointer :bool))
  (x-dimension :uint32)
  (y-dimension :uint32))

;;;   short-cut macros for accessing Tovero C structure fields
(defmacro c-struct-value (value class field)
  (let ((struct-type (string-to-symbol
                      (concat "%tovero-" (string `,class) "-struct"))))
    `(foreign-slot-value ,value '(:struct ,struct-type) ,field)))
(defmacro c-struct-ptr(value class field)
  (let ((struct-type (string-to-symbol
                      (concat "%tovero-" (string `,class) "-struct"))))
    `(foreign-slot-pointer ,value '(:struct ,struct-type) ,field)))

;;;   foreign type conversion

;;;     c++-proxy
(define-foreign-type c++-proxy-type ()
  ((class :initarg :class :reader get-class)
   ;; flag for whether or not the proxy "owns" its underlying pointer
   ;; and is responsible for memory management
   (caller-owns-p :initarg :caller-owns-p
                  :reader get-caller-owns-p
                  :initform nil))
  (:actual-type :pointer))
(defmethod translate-to-foreign ((value c++-proxy)
                                 (type c++-proxy-type))
  (to-foreign value))
(defmethod translate-from-foreign (c++-object
                                   (type c++-proxy-type))
  (if (null-pointer-p c++-object)
      nil
      (let* ((class (get-class type))
             (caller-owns-p (get-caller-owns-p type))
             (instance
              (make-instance class
                             :c++-object c++-object
                             :managingp caller-owns-p)))
        instance)))
(define-parse-method c++-proxy (class &key caller-owns-p)
  (make-instance 'c++-proxy-type :class class :caller-owns-p caller-owns-p))
;;;     interval
(define-foreign-type interval-type ()
  ()
  (:actual-type :pointer))
(define-parse-method interval (class)
  (declare (ignore class))
  (make-instance 'interval-type))
(defmethod translate-to-foreign ((value interval) (type interval-type))
  (let ((pointer (foreign-alloc '(:struct %tovero-interval-struct))))
    (setf (c-struct-value pointer interval 'lower) (f-val (get-lower value)))
    (setf (c-struct-value pointer interval 'upper) (f-val (get-upper value)))
    pointer))
(defmethod translate-from-foreign (pointer (type interval-type))
  (if (null-pointer-p pointer)
      nil
      (let ((lower (c-struct-value pointer interval 'lower))
            (upper (c-struct-value pointer interval 'upper)))
        (interval-2 lower upper))))
(defmethod free-translated-object (pointer (type interval-type) param)
  (declare (ignore param))
  (foreign-free pointer))
;;;     region2
(define-foreign-type region2-type ()
  ()
  (:actual-type :pointer))
(define-parse-method region2 (class)
  (declare (ignore class))
  (make-instance 'region2-type))
(defmethod translate-to-foreign ((value region2) (type region2-type))
  (let ((pointer (foreign-alloc '(:struct %tovero-region2-struct))))
    (setf (c-struct-value pointer region2 'x)
          (translate-to-foreign (get-x value) (make-instance 'interval-type)))
    (setf (c-struct-value pointer region2 'y)
          (translate-to-foreign (get-y value) (make-instance 'interval-type)))
    pointer))
(defmethod translate-from-foreign (pointer (type region2-type))
  (if (null-pointer-p pointer)
      nil
      (let ((x-ptr (c-struct-ptr pointer region2 'x))
            (y-ptr (c-struct-ptr pointer region2 'y)))
        (region2-xy (translate-from-foreign
                     x-ptr (make-instance 'interval-type))
                    (translate-from-foreign
                     y-ptr (make-instance 'interval-type))))))
(defmethod free-translated-object (pointer (type region2-type) param)
  (declare (ignore param))
  (foreign-free pointer))
;;;     region3
(define-foreign-type region3-type ()
  ()
  (:actual-type :pointer))
(define-parse-method region3 (class)
  (declare (ignore class))
  (make-instance 'region3-type))
(defmethod translate-to-foreign ((value region3) (type region3-type))
  (let ((pointer (foreign-alloc '(:struct %tovero-region3-struct))))
    (setf (c-struct-value pointer region3 'x)
          (translate-to-foreign (get-x value) (make-instance 'interval-type)))
    (setf (c-struct-value pointer region3 'y)
          (translate-to-foreign (get-y value) (make-instance 'interval-type)))
    (setf (c-struct-value pointer region3 'z)
          (translate-to-foreign (get-z value) (make-instance 'interval-type)))
    pointer))
(defmethod translate-from-foreign (pointer (type region3-type))
  (if (null-pointer-p pointer)
      nil
      (let ((x-ptr (c-struct-ptr pointer region3 'x))
            (y-ptr (c-struct-ptr pointer region3 'y))
            (z-ptr (c-struct-ptr pointer region3 'z)))
        (region3-xyz (translate-from-foreign
                      x-ptr (make-instance 'interval-type))
                     (translate-from-foreign
                      y-ptr (make-instance 'interval-type))
                     (translate-from-foreign
                      z-ptr (make-instance 'interval-type))))))
(defmethod free-translated-object (pointer (type region3-type) param)
  (declare (ignore param))
  (foreign-free pointer))
;;;     vec3
(defclass vec3 ()
  ((x :reader .x :initarg :x)
   (y :reader .y :initarg :y)
   (z :reader .z :initarg :z)))
(export '.x)
(export '.y)
(export '.z)
(define-foreign-type vec3-type ()
  ()
  (:actual-type :pointer))
(define-parse-method vec3 (class)
  (declare (ignore class))
  (make-instance 'vec3-type))
(defmethod translate-to-foreign ((value vec3) (type vec3-type))
  (let ((pointer (foreign-alloc '(:struct %tovero-vec3-struct))))
    (setf (c-struct-value pointer vec3 'x) (.x value))
    (setf (c-struct-value pointer vec3 'y) (.y value))
    (setf (c-struct-value pointer vec3 'z) (.z value))
    pointer))
(defmethod translate-from-foreign (pointer (type vec3-type))
  (if (null-pointer-p pointer)
      nil
      (let ((x (c-struct-value pointer vec3 'x))
            (y (c-struct-value pointer vec3 'y))
            (z (c-struct-value pointer vec3 'z)))
        (vec3 x y z))))

;;;
;;; C low-level functions
;;;

;;;   struct destructors
(defcfun (%tovero-contours-delete "tovero_contours_delete") :void
  (contours (c++-proxy contours)))
(defcfun (%tovero-mesh-contiguous-delete "tovero_mesh_contiguous_delete") :void
  (mesh (c++-proxy mesh-contiguous)))
(defcfun (%tovero-mesh-delimited-delete "tovero_mesh_delimited_delete") :void
  (mesh (c++-proxy mesh-delimited)))
(defcfun (%tovero-pixels-delete "tovero_pixels_delete") :void
  (pixels (c++-proxy pixels)))
(defcfun (%tovero-voxel-slice-delete "tovero_voxel_slice_delete") :void
  (voxel-slice (c++-proxy voxel-slice)))
;;;   library initialization
(defcfun (%tovero-initialize "tovero_initialize") :void
  (disable-fpe :int))
(defcfun (%tovero-is-initialized "tovero_is_initialized") :int)
;;;   opcode
(defcfun (%tovero-opcode-enum "tovero_opcode_enum") :int
  (op :string))
(defcfun (%tovero-opcode-args "tovero_opcode_args") :int
  (op :int))
;;;   shape
(defcfun (%tovero-tree-x "tovero_tree_x") (c++-proxy shape :caller-owns-p t))
(defcfun (%tovero-tree-y "tovero_tree_y") (c++-proxy shape :caller-owns-p t))
(defcfun (%tovero-tree-z "tovero_tree_z") (c++-proxy shape :caller-owns-p t))

(defcfun (%tovero-tree-var "tovero_tree_var") (c++-proxy shape :caller-owns-p t))
(defcfun (%tovero-tree-is-var "tovero_tree_is_var") :int
  (tree (c++-proxy shape)))

(defcfun (%tovero-tree-const "tovero_tree_const")
    (c++-proxy shape :caller-owns-p t)
  (f :float))
(defcfun (%tovero-tree-get-const "tovero_tree_get_const") :float
  (tree (c++-proxy shape))
  (success (:pointer :int)))

(defcfun (%tovero-tree-constant-vars "tovero_tree_constant_vars")
    (c++-proxy shape :caller-owns-p t)
  (tree (c++-proxy shape)))

(defcfun (%tovero-tree-nonary "tovero_tree_nonary")
    (c++-proxy shape :caller-owns-p t)
  (op :int))
(defcfun (%tovero-tree-unary "tovero_tree_unary")
    (c++-proxy shape :caller-owns-p t)
  (op :int)
  (a (c++-proxy shape)))
(defcfun (%tovero-tree-binary "tovero_tree_binary")
    (c++-proxy shape :caller-owns-p t)
  (op :int)
  (a (c++-proxy shape))
  (b (c++-proxy shape)))

;;; Note: the pointer returned from this function is
;;; the ID of the tree (thus represented by the tree's address)
(defcfun (%tovero-tree-id "tovero_tree_id") (:pointer :void)
  (tree (c++-proxy shape)))

(defcfun (%tovero-tree-eval-r "tovero_tree_eval_r")
    :void
  (tree (c++-proxy shape))
  (region (:pointer (:struct %tovero-region3-struct)))
  (out-interval (:pointer (:struct %tovero-region3-struct))))
(defcfun (%tovero-tree-eval-d "tovero_tree_eval_d")
    :void
  (tree (c++-proxy shape))
  (p (vec3 nil))
  (out-vec (:pointer (:struct %tovero-vec3-struct))))

(defcfun (%tovero-tree-eq "tovero_tree_eq") :int
  (a (c++-proxy shape))
  (b (c++-proxy shape)))

(defcfun (%tovero-tree-delete "tovero_tree_delete") :void
  (tree (c++-proxy shape)))

(defcfun (%tovero-tree-save "tovero_tree_save") :int
  (tree (c++-proxy shape))
  (file-name :string))
(defcfun (%tovero-tree-load "tovero_tree_load")
    (c++-proxy shape :caller-owns-p t)
  (file-name :string))

(defcfun (%tovero-tree-remap "tovero_tree_remap")
    (c++-proxy shape :caller-owns-p t)
  (p (c++-proxy shape))
  (x (c++-proxy shape))
  (y (c++-proxy shape))
  (z (c++-proxy shape)))

(defcfun (%tovero-tree-bounds "tovero_tree_bounds")
    :void
  (tree (c++-proxy shape))
  (out-interval (:pointer (:struct %tovero-region3-struct))))

(defcfun (%tovero-tree-print "tovero_tree_print")
    :string
  (tree (c++-proxy shape)))

(defcfun (%tovero-tree-render-slice "tovero_tree_render_slice")
    (c++-proxy contours :caller-owns-p t)
  (tree (c++-proxy shape))
  (region (region2 nil))
  (z :float)
  (resolution :float))

(defcfun (%tovero-tree-save-slice "tovero_tree_save_slice")
    :void
  (tree (c++-proxy shape))
  (region (region2 nil))
  (z :float)
  (resolution :float)
  (file-name :string))

(defcfun (%tovero-tree-render-mesh-contiguous
          "tovero_tree_render_mesh_contiguous")
    (c++-proxy mesh-contiguous :caller-owns-p t)
  (tree (c++-proxy shape))
  (region (region3 nil))
  (resolution :float))

(defcfun (%tovero-tree-render-mesh-delimited
          "tovero_tree_render_mesh_delimited")
    (c++-proxy mesh-delimited :caller-owns-p t)
  (tree (c++-proxy shape))
  (region (region3 nil))
  (resolution :float))

(defcfun (%tovero-tree-save-mesh "tovero_tree_save_mesh")
    :int
  (tree (c++-proxy shape))
  (region (region3 nil))
  (resolution :float)
  (file-name :string))

(defcfun (%tovero-tree-render-pixels "tovero_tree_render_pixels")
    (c++-proxy pixels :caller-owns-p t)
  (tree (c++-proxy shape))
  (region (region2 nil))
  (z :float)
  (resolution :float))

(defcfun (%tovero-tree-render-voxel-slice "tovero_tree_render_voxel_slice")
    (c++-proxy voxel-slice :caller-owns-p t)
  (tree (c++-proxy shape))
  (region (region2 nil))
  (z :float)
  (resolution :float)
  (voxel-off-value :uint8)
  (voxel-on-value :uint8))

;;   renderer (libfive) versioning
(defcfun (%tovero-renderer-version "tovero_renderer_version") :string)
(defcfun (%tovero-renderer-revision "tovero_renderer_revision") :string)

;;; protocol
(defgeneric get-value (self))
(export 'get-value)

;;;
;;; Class methods
;;;

;;;   interval class
(defgeneric interval (lower-upper-pair))
(export 'interval)
(defmethod interval ((lower-upper-pair cons))
  (make-instance 'interval
                 :lower (car lower-upper-pair)
                 :upper (cdr lower-upper-pair)))
(defmethod interval-2 ((lower number)
                       (upper number))
  (make-instance 'interval :lower lower :upper upper))
(defmethod get-value ((self interval))
  (cons (get-lower self) (get-upper self)))
(defmethod print-object ((self interval) stream)
  (format stream "(INTERVAL '(~s . ~s))"
          (get-lower self) (get-upper self)))

;;;   region2 class
(defgeneric region2 (region-list))
(export 'region2)
(defmethod region2 ((region-list list))
  (destructuring-bind (x y &rest etc) region-list
    (make-instance 'region2
                   :x (interval x)
                   :y (interval y))))
(defmethod region2-xy ((x interval)
                       (y interval))
  (make-instance 'region2 :x x :y y))
(defmethod get-value ((self region2))
  (list (get-value (get-x self))
        (get-value (get-y self))))
(defmethod print-object ((self region2) stream)
  (format stream "(REGION2 ~s)" (get-value self)))

;;;   region3 class
(defgeneric region3 (region-list))
(export 'region3)
(defmethod region3 ((region-list list))
  (destructuring-bind (x y z &rest etc) region-list
    (make-instance 'region3
                   :x (interval x)
                   :y (interval y)
                   :z (interval z))))
(defmethod region3-xyz ((x interval)
                        (y interval)
                        (z interval))
  (make-instance 'region3 :x x :y y :z z))
(defmethod get-value ((self region3))
    (list (get-value (get-x self))
          (get-value (get-y self))
          (get-value (get-z self))))
(defmethod print-object ((self region3) stream)
  (format stream "(REGION3 ~s)" (get-value self)))

;;; protocol
(defgeneric get-count (self))
(export 'get-points)

;;;   contour class
(defgeneric get-points (self))
(export 'get-points)
(defmethod get-points ((self contour))
   (c-struct-value (to-foreign self) contour 'points))
(defmethod get-count ((self contour))
   (c-struct-value (to-foreign self) contour 'count))
(defmethod print-object ((self contour) stream)
  (format stream "#<CONTOUR ~a [POINT COUNT: ~a]>"
          (to-foreign self) (get-count self)))

;;;   contours class
(defmethod destructor ((self contours)) '%tovero-contours-delete)
(defgeneric get-count (self))
(export 'get-count)
(defmethod get-count ((self contours))
  (c-struct-value (to-foreign self) contours 'count))
(defgeneric get-contour (self i))
(export 'get-contour)
(defmethod get-contour ((self contours)
                        (i integer))
  (let ((contours (c-struct-value (to-foreign self) contours 'contours)))
    (make-instance 'contour
                   :c++-object (mem-aref contours '%tovero-contour-struct i))))
(defmethod print-object ((self contours) stream)
  (format stream "#<CONTOURS ~a [CONTOUR COUNT: ~a]>"
          (to-foreign self) (get-count self)))

;;; mesh class
;;;   protocol
(defgeneric get-vertices (self))
(export 'get-vertices)
(defgeneric get-vertex-count (self))
(export 'get-vertex-count)

;;;   mesh-contiguous class
(defmethod destructor ((self mesh-contiguous)) '%tovero-mesh-contiguous-delete)
(defmethod get-vertices ((self mesh-contiguous))
  (c-struct-value (to-foreign self) mesh-contiguous 'vertices))
(defmethod get-vertex-count ((self mesh-contiguous))
  (c-struct-value (to-foreign self) mesh-contiguous 'vertex-count))
(defgeneric get-triangle-indices (self))
(export 'get-triangle-indices)
(defmethod get-triangle-indices ((self mesh-contiguous))
  (c-struct-value (to-foreign self) mesh-contiguous 'triangle-indices))
(defgeneric get-triangle-count (self))
(export 'get-triangle-count)
(defmethod get-triangle-count ((self mesh-contiguous))
  (c-struct-value (to-foreign self) mesh-contiguous 'triangle-count))
(defmethod print-object ((self mesh-contiguous) stream)
  (format
   stream
   "#<MESH-CONTIGUOUS ~a [VERTEX COUNT: ~a TRIANGLE COUNT: ~a]>"
   (to-foreign self)
   (get-vertex-count self)
   (get-triangle-count self)))

;;;   mesh-delimited class
(defmethod destructor ((self mesh-delimited)) '%tovero-mesh-delimited-delete)
(defmethod get-vertices ((self mesh-delimited))
  (c-struct-value (to-foreign self) mesh-delimited 'vertices))
(defmethod get-vertex-count ((self mesh-delimited))
  (c-struct-value (to-foreign self) mesh-delimited 'vertex-count))
(defgeneric get-coord-indices (self))
(export 'get-coord-indices)
(defmethod get-coord-indices ((self mesh-delimited))
  (c-struct-value (to-foreign self) mesh-delimited 'coord-indices))
(defgeneric get-coord-index-count (self))
(export 'get-coord-index-count)
(defmethod get-coord-index-count ((self mesh-delimited))
  (c-struct-value (to-foreign self) mesh-delimited 'coord-index-count))
(defmethod print-object ((self mesh-delimited) stream)
  (format
   stream
   "#<MESH-DELIMITED ~a [VERTEX COUNT: ~a COORD INDEX COUNT: ~a]>"
   (to-foreign self)
   (get-vertex-count self)
   (get-coord-index-count self)))

;;;   pixels class
(defmethod destructor ((self pixels)) '%tovero-pixels-delete)
(defgeneric get-pixels (self))
(export 'get-pixels)
(defmethod get-pixels ((self pixels))
   (c-struct-value (to-foreign self) pixels 'pixels))
(defgeneric get-width (self))
(export 'get-width)
(defmethod get-width ((self pixels))
   (c-struct-value (to-foreign self) pixels 'width))
(defgeneric get-height (self))
(export 'get-height)
(defmethod get-height ((self pixels))
   (c-struct-value (to-foreign self) pixels 'height))
(defmethod print-object ((self pixels) stream)
  (format stream "#<PIXELS ~a [WIDTH: ~a HEIGHT: ~a]>"
          (to-foreign self) (get-width self) (get-height self)))

;;;   voxel-slice class
(defmethod destructor ((self voxel-slice)) '%tovero-voxel-slice-delete)
(defgeneric get-voxels (self))
(export 'get-voxels)
(defmethod get-voxels ((self voxel-slice))
   (c-struct-value (to-foreign self) voxel-slice 'voxels))
(defgeneric get-x-dimension (self))
(export 'get-x-dimension)
(defmethod get-x-dimension ((self voxel-slice))
   (c-struct-value (to-foreign self) voxel-slice 'x-dimension))
(defgeneric get-y-dimension (self))
(export 'get-y-dimension)
(defmethod get-y-dimension ((self voxel-slice))
   (c-struct-value (to-foreign self) voxel-slice 'y-dimension))
(defmethod print-object ((self voxel-slice) stream)
  (format stream "#<VOXEL-SLICE ~a [X-DIMENSION: ~a Y-DIMENSION: ~a]>"
          (to-foreign self) (get-x-dimension self) (get-y-dimension self)))

;;;   shape class
(defmethod destructor ((self shape)) '%tovero-tree-delete)

;;;
;;; Library functions
;;;

;;;   initialization
(defun tovero-initialize (&key (disable-fpe t))
  (%tovero-initialize (boolean-to-c-int disable-fpe)))
(export 'tovero-initialize)
(defun tovero-initialized-p () (c-int-to-boolean (%tovero-is-initialized)))
(export 'tovero-initialized-p)

;;;   opcode
(defgeneric opcode-enum (op))
(export 'opcode-enum)
(defmethod opcode-enum ((op string))
  (%tovero-opcode-enum op))
(defgeneric opcode-args (op))
(export 'opcode-args)
(defmethod opcode-args((op integer))
  (%tovero-opcode-args op))

;;;   shape
(defun shape-x () (%tovero-tree-x))
(export 'shape-x)
(defun shape-y () (%tovero-tree-y))
(export 'shape-y)
(defun shape-z () (%tovero-tree-z))
(export 'shape-z)

(defun shape-var () (%tovero-tree-var))
(export 'shape-var)
(defgeneric shape-var-p (shape))
(export 'shape-var-p)
(defmethod shape-var-p ((shape shape))
  (c-int-to-boolean (%tovero-tree-is-var shape)))

(defgeneric shape-const (f))
(export 'shape-const)
(defmethod shape-const ((f number)) (%tovero-tree-const (f-val f)))
(defgeneric shape-get-const (shape))
(export 'shape-get-const)
(defmethod shape-get-const ((shape shape))
  (with-foreign-object (success :int)
    (let ((value (%tovero-tree-get-const shape success)))
      (values value (c-int-to-boolean (mem-ref success :int))))))

(defgeneric shape-constant (shape))
(export 'shape-constant)
(defmethod shape-constant ((shape shape)) (%tovero-tree-constant-vars shape))

(defgeneric shape-nonary (op))
(export 'shape-nonary)
(defmethod shape-nonary ((op integer)) (%tovero-tree-nonary op))
(defgeneric shape-unary (op a))
(export 'shape-unary)
(defmethod shape-unary ((op integer)
                        (a shape))
  (%tovero-tree-unary op a))
(defgeneric shape-binary (op a b))
(export 'shape-binary)
(defmethod shape-binary ((op integer)
                         (a shape)
                         (b shape))
  (%tovero-tree-binary op a b))

(defgeneric shape-tree-id (shape))
(export 'shape-tree-id)
(defmethod shape-tree-id ((shape shape))
  (pointer-address (%tovero-tree-id shape)))
(defmethod shape-equal-p ((a shape)
                          (b shape))
  (c-int-to-boolean (%tovero-tree-eq a b)))
(export 'shape-equal-p)

(defgeneric shape-save (shape file-name))
(export 'shape-save)
(defmethod shape-save ((shape shape)
                       (file-name string))
  (%tovero-tree-save shape file-name))
(defgeneric shape-load (file-name))
(export 'shape-load)
(defmethod shape-load ((file-name string))
  (%tovero-tree-load file-name))

(defgeneric shape-remap (shape x y z))
(export 'shape-map)
(defmethod shape-remap ((shape shape)
                        (x shape)
                        (y shape)
                        (z shape))
  (%tovero-tree-remap shape x y z))

(defgeneric shape-bounds (shape))
(export 'shape-bounds)
(defmethod shape-bounds ((shape shape))
  (warn "Called 'shape-bounds':  Abandon all hope, ye who enter here.")
  (with-foreign-object (bounds-ptr '(:struct %tovero-region3-struct))
    (%tovero-tree-bounds shape bounds-ptr)
    (translate-from-foreign bounds-ptr (make-instance 'region3-type))))

(defgeneric shape-slice (shape region z resolution &optional file-name))
(export 'shape-slice)
(defmethod shape-slice ((shape shape)
                        (region region2)
                        (z number)
                        (resolution number)
                        &optional file-name)
  (if (null file-name)
      (%tovero-tree-render-slice shape region
                                 (f-val z) (f-val resolution))
      (%tovero-tree-save-slice shape region
                               (f-val z) (f-val resolution)
                               file-name)))
(defmethod shape-slice ((shape shape)
                        (region list)
                        (z number)
                        (resolution number)
                        &optional file-name)
  (if (null file-name)
      (%tovero-tree-render-slice shape (region2 region)
                                 (f-val z) (f-val resolution))
      (%tovero-tree-save-slice shape (region2 region)
                               (f-val z) (f-val resolution)
                               file-name)))
(defgeneric shape-to-mesh (shape region resolution
                           &key file-name mesh-type))
(export 'shape-to-mesh)
(defmethod shape-to-mesh ((shape shape)
                          (region region3)
                          (resolution number)
                          &key
                            file-name
                            (mesh-type :tovero-mesh-delimited))
  (if (null file-name)
      (case mesh-type
        (:tovero-mesh-contiguous
         (%tovero-tree-render-mesh-contiguous shape
                                              region
                                              (f-val resolution)))
        (:tovero-mesh-delimited
         (%tovero-tree-render-mesh-delimited shape
                                             region
                                             (f-val resolution)))
        (otherwise (error "Invalid mesh type: ~a." mesh-type)))
      (%tovero-tree-save-mesh shape
                               region
                               (f-val resolution)
                               file-name)))
(defmethod shape-to-mesh ((shape shape)
                          (region list)
                          (resolution number)
                          &key
                            file-name
                            (mesh-type :tovero-mesh-delimited))
  (shape-to-mesh shape (region3 region) resolution
                 :file-name file-name
                 :mesh-type mesh-type))

(defgeneric shape-slice-pixels (shape region z resolution))
(export 'shape-slice-pixels)
(defmethod shape-slice-pixels ((shape shape)
                               (region region2)
                               (z number)
                               (resolution number))
  (%tovero-tree-render-pixels shape
                              region
                              (f-val z)
                              (f-val resolution)))
(defmethod shape-slice-pixels ((shape shape)
                               (region list)
                               (z number)
                               (resolution number))
  (%tovero-tree-render-pixels shape
                              (region2 region)
                              (f-val z)
                              (f-val resolution)))

(defgeneric shape-slice-voxels (shape region z resolution
                                voxel-off-value voxel-on-value))
(export 'shape-slice-voxels)
(defmethod shape-slice-voxels ((shape shape)
                               (region region2)
                               (z number)
                               (resolution number)
                               (voxel-off-value integer)
                               (voxel-on-value integer))
  (%tovero-tree-render-voxel-slice shape
                                   region
                                   (f-val z)
                                   (f-val resolution)
                                   voxel-off-value
                                   voxel-on-value))
(defmethod shape-slice-voxels ((shape shape)
                               (region list)
                               (z number)
                               (resolution number)
                               (voxel-off-value integer)
                               (voxel-on-value integer))
  (%tovero-tree-render-voxel-slice shape
                                   (region2 region)
                                   (f-val z)
                                   (f-val resolution)
                                   voxel-off-value
                                   voxel-on-value))

(defgeneric shape-print (shape))
(export 'shape-print)
(defmethod shape-print ((shape shape)) (%tovero-tree-print shape))

;;;   shape constructor

(defgeneric number-to-shape (n))
(export 'number-to-shape)
(defmethod number-to-shape ((n number)) (%tovero-tree-const (f-val n)))

(defgeneric ensure-shape (shape))
(export 'ensure-shape)
(defmethod ensure-shape ((shape shape)) shape)
(defmethod ensure-shape ((n number)) (number-to-shape n))

(defgeneric shape (op &optional a b))
(export 'shape)
(defmethod shape ((op symbol)
                  &optional
                    (a nil a-supplied-p)
                    (b nil b-supplied-p))
  (flet
      ((check-num-args-required (op num-args-required num-args-supplied)
         (when (/= num-args-required num-args-supplied)
           (signal 'wrong-num-args
                   (concat
                    "Wrong number of arguments: ~a supplied to shape function"
                    " op: '~a' requires: ~a.")
                   op
                   num-args-supplied
                   num-args-required))))
    (let* ((str (string op))
           (opcode (opcode-enum str)))
      (when (= opcode -1)
        (signal 'wrong-type-arg "Invalid opcode: '~a'." str))
      (let* ((num-args-required (opcode-args opcode))
             (num-args-supplied (if a-supplied-p (if b-supplied-p 2 1) 0)))
        (check-num-args-required str num-args-required num-args-supplied)
        (case num-args-required
          (0 (shape-nonary opcode))
          (1 (shape-unary opcode (ensure-shape a)))
          (2 (shape-binary opcode
                           (ensure-shape a)
                           (ensure-shape b)))
          (otherwise
           (signal 'assertion-failed
                   "Invalid number of args: ~a required for opcode: '~a"
                   num-args-required
                   str)))))))

;;;   additional shape methods
(defgeneric shapep (shape))
(export 'shapep)
(defmethod shapep ((shape shape)) t)
(defmethod shapep ((obj t)) nil)

(defgeneric varp (shape))
(export 'varp)
(defmethod varp ((shape shape))
  (c-int-to-boolean (%tovero-tree-is-var shape)))
(defmethod varp ((obj t)) nil)

(defgeneric shape-eval (shape point))
(export 'shape-eval)
(defmethod shape-eval ((shape shape)
                       (point vec3))
  (let ((out (shape-remap shape
                          (ensure-shape (.x point))
                          (ensure-shape (.y point))
                          (ensure-shape (.z point)))))
    (multiple-value-bind (value is-const-p)
        (shape-get-const out)
      (if is-const-p
          value
          out))))
(defgeneric shape-eval-region (shape region))
(export 'shape-eval-region)
(defmethod shape-eval-region ((shape shape)
                              (region region3))
  (with-foreign-object (interval-ptr '(:struct %tovero-interval-struct))
    (%tovero-tree-eval-r shape region interval-ptr)
    (translate-from-foreign interval-ptr (make-instance 'interval-type))))
(defgeneric shape-derivs (shape point))
(export 'shape-defivs)
(defmethod shape-derivs ((shape shape)
                         (point vec3))
  (with-foreign-object (deriv-ptr '(:struct %tovero-vec3-struct))
    (%tovero-tree-eval-d shape point deriv-ptr)
    (translate-from-foreign deriv-ptr (make-instance 'vec3-type))))

;;;   shape macros
(defmacro lambda-shape (vars &body body)
  `((lambda ,vars
      (declare (ignorable ,@vars))  ; prevent pesky "unused" warnings
      ,@body)
    (shape 'var-x)
    (shape 'var-y)
    (shape 'var-z)))
(export 'lambda-shape)

(defmacro define-shape ((name &rest vars) &body body)
  `(defparameter ,name (lambda-shape ,vars ,@body)))
(export 'define-shape)

(defmacro remap-shape ((shape . vars) x y z)
  `(shape-eval ,shape
               (vec3 (lambda-shape ,vars ,x)
                     (lambda-shape ,vars ,y)
                     (lambda-shape ,vars ,z))))
(export 'remap-shape)
