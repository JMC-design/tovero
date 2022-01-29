#| tovero-viewer.lisp

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

(defpackage #:tovero-viewer
  (:use :cl :cffi)
  (:export nil))

(in-package #:tovero-viewer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or sbcl ecl)
  (error "This Common Lisp distribution is not supported by Tovero Viewer."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; try to figure out a good size for ptrdiff_t
  (cond
    ((= (foreign-type-size :pointer)
        (foreign-type-size :long))
     (defctype ptrdiff-t :long ))
    ((= (foreign-type-size :pointer)
        (foreign-type-size :long-long))
     (defctype ptrdiff-t :long-long))
    (t (error "not sure how big ptrdiff-t is on this platform"))))
(defctype sizeiptr ptrdiff-t)

;;; missing OpenGL elements (prefixed with 'gl/')
;;;   types
(defctype sizei :unsigned-int)
;;;   enum
(defcenum (gl/enum :unsigned-int)
  (:array-buffer #x8892)
  (:float #x1406)
  (:triangles #x4)
  (:unsigned-int #x1405)
  (:element-array-buffer #x8893)
  (:static-draw #x88E4))
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; try to figure out a good size for ptrdiff_t
  (cond
    ((= (foreign-type-size :pointer)
        (foreign-type-size :long))
     (defctype ptrdiff-t :long ))
    ((= (foreign-type-size :pointer)
        (foreign-type-size :long-long))
     (defctype ptrdiff-t :long-long))
    (t (error "not sure how big ptrdiff-t is on this platform"))))
(defctype sizeiptr ptrdiff-t)
;;;   functions
(defcfun (%gl-vertex-pointer "glVertexPointer") :void
  (size :int)
  (type :unsigned-int)
  (stride sizei)
  (pointer :pointer))
(declaim (inline gl/vertex-pointer))
(defun gl/vertex-pointer (size type &key (stride 0) (pointer (null-pointer)))
  (let ((type-value (foreign-enum-value 'gl/enum type)))
    (%gl-vertex-pointer size type-value stride pointer)))
;;;     glCreateProgram
(declaim (inline gl/create-program))
(defun gl/create-program () (%gl:create-program))
;;;     glDrawElements
(defcfun (%gl-draw-elements "glDrawElements") :void
  (mode :unsigned-int)
  (count sizei)
  (type :unsigned-int)
  (indices :pointer))
(declaim (inline gl/draw-elements))
(defun gl/draw-elements (mode count type &key (indices (null-pointer)))
  (let ((mode-value (foreign-enum-value 'gl/enum mode))
        (type-value (foreign-enum-value 'gl/enum type)))
    (%gl-draw-elements mode-value count type-value indices)))
;;;     glBufferData
(defcfun (%gl-buffer-data "glBufferData") :void
  (target :unsigned-int)
  (size sizeiptr)
  (data (:pointer :void))
  (usage :unsigned-int))
(declaim (inline gl/buffer-data))
(defun gl/buffer-data (target size data usage)
  (let ((target-value (foreign-enum-value 'gl/enum target))
        (usage-value (foreign-enum-value 'gl/enum usage)))
    (%gl-buffer-data target-value size data usage-value)))
