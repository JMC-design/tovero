#| shader.lisp

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

(in-package #:tovero-viewer)

(defclass shader ()
  ((vertex-shader)
   (fragment-shader)
   (shader-program)))

;;; shader source
;;;   vertex
(defparameter *vertex-shader-source*
"#version 120
varying vec3 ec_pos;

void main() {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

    ec_pos = gl_Position.xyz;
}
")
;;;   fragment
(defparameter *fragment-shader-source*
"#version 120

varying vec3 ec_pos;

void main() {
    vec3 base3 = vec3(0.99, 0.96, 0.89);
    vec3 base2 = vec3(0.92, 0.91, 0.83);
    vec3 base00 = vec3(0.40, 0.48, 0.51);

    vec3 ec_normal = normalize(cross(dFdx(ec_pos), dFdy(ec_pos)));

    float a = dot(ec_normal, vec3(0.0, 0.0, 1.0));
    float b = dot(ec_normal, vec3(-0.57, -0.57, 0.57));

    gl_FragColor = vec4((a*base2 + (1-a)*base00)*0.5 +
                        (b*base3 + (1-b)*base00)*0.5, 1.0);
}
")

(defun print-shader-log (shader shader-type)
    (format t "~%*** ~a shader compile failed!~%" shader-type)
    (format t "*** Log:~%")
    (format t "~a~%" (gl:get-shader-info-log shader))
    (format t "***~%"))

(defun print-program-log (program)
    (format t "~%*** Program link failed!~%")
    (format t "*** Log:~%")
    (format t "~a~%" (gl:get-program-info-log program))
    (format t "***~%"))

(defun shader ()
  (let ((shader (make-instance 'shader)))
    (let ((vert-shader (gl:create-shader :vertex-shader))
          (frag-shader (gl:create-shader :fragment-shader)))
      ;; compile the vertex and fragment shaders
      ;;   vertex shader
      (gl:shader-source vert-shader *vertex-shader-source*)
      (gl:compile-shader vert-shader)
      (if (gl:get-shader vert-shader :compile-status)
          (setf (slot-value shader 'vertex-shader) vert-shader)
          (print-shader-log vert-shader "Vertex"))
      ;;   fragment shader
      (gl:shader-source frag-shader *fragment-shader-source*)
      (gl:compile-shader frag-shader)
      (if (gl:get-shader frag-shader :compile-status)
          (setf (slot-value shader 'fragment-shader) frag-shader)
          (print-shader-log frag-shader "Fragment")))

    ;; attach the shaders to the program and link
    (let ((vertex-shader (slot-value shader 'vertex-shader))
          (fragment-shader (slot-value shader 'fragment-shader)))
      (unless (or (null vertex-shader) (null fragment-shader))
        ;; attach shaders and link shader program
        (let ((program (gl:create-program)))
          (gl:attach-shader program vertex-shader)
          (gl:attach-shader program fragment-shader)
          (gl:link-program program)
          (if (gl:get-program program :link-status)
              (progn
                (gl:use-program program)
                (setf (slot-value shader 'shader-program) program))
              (print-program-log program)))))
    shader))

(defgeneric cleanup (self))
(defmethod cleanup ((self shader))
  ;; clean up the shaders
  (let ((vertex-shader (slot-value self 'vertex-shader)))
    (unless (null vertex-shader) (gl:delete-shader vertex-shader)))
  (let ((fragment-shader (slot-value self 'fragment-shader)))
    (unless (null fragment-shader) (gl:delete-shader fragment-shader)))
  (let ((shader-program (slot-value self 'shader-program)))
    (unless (null shader-program) (gl:delete-program shader-program))))
