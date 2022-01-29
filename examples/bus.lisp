#| bus.lisp

Copyright 2018 Kavalogic, Inc.
Copyright 2013-2014 Roan Trail, Inc.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

  (1) Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  (2) Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in
  the documentation and/or other materials provided with the
  distribution.

  (3) The name of the author may not be used to
  endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

Derived from the (Roan Trail) Tovero 0.2.0 file:
  ./src/tovero/graphics/models/bus.rb

Original License: BSD 3-clause

|#

;;; TODO: make sub-assemblies separate meshes for different materials

;;; TODO: use mirror for symmetric assemblies such as R/L headlights,
;;; TODO: windows, etc.
;;; TODO: use 'shell'
;;; TODO: 'move' -> 'translate'

(require 'clive-viewer)
(require 'tovero)

(tovero:use-tovero-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clive:define-scene-type directional-light)
  (clive:define-scene-type perspective-camera)
  (clive:define-scene-type separator)
  (clive:define-scene-type material)
  (clive:define-scene-type shape-hints)
  (clive:define-scene-type draw-style)
  (clive:define-scene-type cube)
  (clive:define-scene-type light-model)
  (clive:define-scene-type translation)
  (clive:define-scene-type indexed-face-set)
  (clive:define-scene-type vertex-property))

;;; Note: units of distance in 'meters'

;;;
;;; helper functions
;;;

(defun deg-to-rad (angle-deg) (* (/ angle-deg 180) pi))

;;;
;;; materials for parts
;;;

(defparameter *rubber-material*
  (clive:material :ambient-color (color :v #(0.00784 0.00784 0.00784))
                  :diffuse-color  (color :v #(0.0235 0.0235 0.0235))
                  :shininess 0.0))

(defparameter *trim-material*
  (clive:material :ambient-color (color :v #(0.3 0.3 0.3))
                  :diffuse-color (color :v #(0.7 0.7 0.7))
                  :specular-color (color :v #(0.3 0.3 0.3))
                  :shininess 0.9))

(defparameter *glass-material*
  (clive:material :ambient-color (color :v #(0.16 0.17 0.17))
                  :diffuse-color (color :v #(0.625 0.69 0.69))
                  :shininess 0.4
                  :transparency 0.8))

(defparameter *body-material*
  (clive:material :ambient-color (color :v #(0 0.125 .2))
                  :diffuse-color (color :v #(0 0.50 0.8))
                  :specular-color (color :v #(0 0.125 .2))
                  :shininess 0.2))


;;;
;;; bus-shape class
;;;

(defclass bus-shape ()
  (;; instance slots
   (parent :initarg :parent :reader parent)
   bounds
   shape
   material
   rendering))

(defgeneric setup-shape (self))
(defgeneric calculate-dimensions (self))
(defgeneric render (self))
(defmethod render ((self bus-shape))
  (with-slots (bounds shape resolution material) self
    (let* ((mesh (shape-to-mesh shape bounds resolution)))
      (setf rendering
            (clive:separator
             material
             (clive:vertex-property
              :vertex (clive:foreign-vertex-array
                       (tovero:get-vertex-count mesh)
                       (tovero:get-vertices mesh)))
             (clive:indexed-face-set
              :coord-index (clive:foreign-coord-index-array
                            (tovero:get-coord-index-count mesh)
                            (tovero:get-coord-indices mesh))))))))


;;
;; dish class
;;

(defclass dish (bus-shape)
  (;; specifications
   (curvature-radius :initarg :curvature-radius)
   (disk-radius :initarg :disk-radius)
   (direction :initarg :direction)
   (up :initarg :up)
   (offset :initarg :offset :initform #[0 0 0])))

(defmethod setup-shape ((self dish))
  (with-slots (curvature-radius disk-radius direction up offset
               shape)
      self
    (let* ((x (sqrt (- (expt curvature-radius 2)
                       (expt disk-radius 2))))
           (basis (cross direction up))
           (center-offset (* direction x))
           (sphere (sphere curvature-radius (- offset center-offset)))
           (direction-offset (* direction curvature-radius))
           (up-offset (* up curvature-radius))
           (basis-offset (* basis curvature-radius))
           (radius-offset (+ direction-offset up-offset basis-offset))
           (box (box-dim (- offset center-offset radius-offset)
                         (+ direction-offset center-offset)
                         (* up-offset 2)
                         (* basis-offset 2))))
      (setf shape (difference sphere box)))))


;;;
;;; bus-part class
;;;

(defclass bus-part ()
  (;; instance slots
   (parent :initarg :parent :reader parent)
   bounds
   shape
   material))


;;;
;;; bus-window class
;;;

(defclass bus-window (bus-part)
  (;; specifications
   (glass-thickness :initform 0.00635)))


;;;
;;; bus bumper class
;;;

(defclass bus-bumper (bus-part)
  (;; specifications
   (radius :initform 0.125)
   (body-gap :initform 0.03)
   flattening-offset))

(defmethod calculate-dimensions ((self bus-bumper))
  (with-slots (flattening-offset radius) self
    (setf flattening-offset (/ radius 4))))

(defmethod setup-shape ((self bus-bumper))
  (with-slots (parent
               radius body-gap flattening-offset)
      self
    (let* ((cross-member-length (+ (with-slots (width) parent width)
                                   (* body-gap 2)
                                   (- (* flattening-offset 2))))
           (cross-member (cylinder-z radius cross-member-length))
           (sphere (sphere radius))
           (extension-length radius)
           (extension (cylinder-vec radius #[extension-length 0 0]))
           (box (box #[(- flattening-offset)
                       (* radius -2)
                       (- flattening-offset)]
                     #[(+ extension-length
                          flattening-offset
                          radius)
                       (* radius 2)
                       (+ cross-member-length flattening-offset)]))
           (min-x (- radius)) (max-x (+ extension-length radius))
           (min-y (- radius)) (max-y radius)
           (min-z (- radius)) (max-z (+ cross-member-length radius)))
      (with-slots (shape bounds resolution) self
        (setf shape
              (difference
               (union cross-member
                      sphere
                      extension
                      (move sphere #[0 0 cross-member-length])
                      (move extension #[0 0 cross-member-length]))
               box))
        (setf bounds
              (region3 (list (cons min-x max-x)
                             (cons min-y max-y)
                             (cons min-z max-z))))
        (with-slots (bus-resolution resolution) parent
          (setf resolution bus-resolution))


;;;
;;; bus hub cap class
;;;

(defclass bus-hub-cap (bus-part)
  (curvature-radius
   disk-radius
   direction
   up
   base))

(defmethod setup-shape ((self bus-hub-cap))
  (with-slots (parent
               shape
               curvature-radius disk-radius
               direction up
               base)
      self
    (let ((dish (make-instance
                   'dish
                   :curvature-radius curvature-radius
                   :disk-radius disk-radius
                   :direction direction
                   :up up
                   :offset base
                   :parent self)))
      (setf shape (setup-shape dish))
      (setf bounds (with-slots (bounds) dish bounds))
      (setf resolution (with-slots (resolution) parent resolution)))))

(defclass bus-tire (bus-part)
  (;; specifications (185/80R14 tires)
   (width :initform 0.185)
   (sidewall-ratio :initform 0.8)
   (center-diameter :initform 0.356)
   ;; calculated dimensions
   sidewall-height
   height
   inner-center-cylinder
   outer-center-cylinder))

(defmethod calculate-dimensions ((self bus-tire))
  (with-slots (sidewall-ratio
               center-diameter
               sidewall-height width height
               inner-center-cylinder outer-center-cylinder)
      self
    (setf sidewall-height (* width sidewall-ratio))
    (setf height (+ center-diameter (* sidewall-height 2)))
    (setf outer-center-cylinder (cylinder-vec (/ height 2)
                                              #[0 0 (* width 2)]
                                              #[0 0 (- width)]))
    (setf inner-center-cylinder (cylinder-vec (/ center-diameter 2)
                                              #[0 0 (* width 2)]
                                              #[0 0 (- width)]))))

(defmethod setup-shape ((self bus-tire))
  (with-slots (shape width height
               inner-center-cylinder outer-center-cylinder)
      self
    (let* ((torus-inner-radius (/ (* width 1.1) 2))
           (torus-outer-radius (- (/ (* height 1.05) 2) torus-inner-radius))
           (torus (torus-z torus-outer-radius
                           torus-inner-radius)))
      (setf shape (difference
                   (intersection torus
                                 outer-center-cylinder)
                   inner-center-cylinder))
      (setf bounds
            (region3 (list (cons (- torus-outer-radius) torus-outer-radius)
                           (cons (- torus-outer-radius) torus-outer-radius)
                           (cons (- torus-inner-radius) torus-inner-radius))))
      (setf resolution (with-slots (resolution) parent resolution)))))


;;
;; bus wheel rim class
;;

(defclass bus-wheel-rim (bus-part)
  (;; dimensions
   diameter
   width
   rim-offset))

(defmethod setup-shape ((self bus-wheel-rim))
  (with-slots (shape diameter width rim-offset)
      self
    (let ((radius (/ diameter 2))
          (wheel-rim-depth/2 (/ (/ width 1.1) )))
      (setf rim-offset #[0 0 wheel-rim-depth])
      (setf shape (cylinder-vec radius
                                rim-offset
                                #[0 0 (- wheel-rim-depth/2)]))
      (setf bounds
            (region3 (list (cons (- radius) radius)
                           (cons (- radius) radius)
                           (cons (- wheel-rim-depth/2) wheel-rim-depth/2))))
      (setf resolution (with-slots (resolution) parent resolution)))))


;;
;; bus wheel class
;;

(defclass bus-wheel (bus-assembly)
  (;; dependent dimensions
   height
   ;; parts
   tire
   wheel-rim
   hub-cap))

(defmethod initialize-instance :after ((self bus-wheel) &key)
  (with-slots (tire wheel-rim hub-cap) self
    (setf tire (make-instance 'bus-tire :parent self))
    (setf wheel-rim (make-instance 'bus-wheel-rim :parent self))
    (setf hub-cap (make-instance 'bus-hub-cap :parent self))))

(defmethod calculate-dimensions ((self bus-wheel))
  (with-slots (tire height wheel-rim hub-cap)
      self
    ;; update part dimensions
    (calculate-dimensions tire)
    ;; update dependent dimensions
    (with-slots ((tire-diameter diameter)
                 (tire-center-diameter center-diameter)
                 (tire-width width)
                 (tire-height height))
        tire
      (setf height tire-height)
      (with-slots ((rim-diameter diameter) (rim-width width)) wheel-rim
        (setf rim-diameter tire-center-diameter)
        (setf rim-width tire-width))
      (with-slots ((hub-cap-disk-radius disk-radius)
                   (hub-cap-curvature-radius curvature-radius))
          hub-cap
        (setf hub-cap-disk-radius (* (/ tire-center-diameter 2) 9/10))
        (setf hub-cap-curvature-radius (* hub-cap-disk-radius 9/5))))))

(defmethod setup-shape ((self bus-wheel))
  (with-slots (tire wheel-rim hub-cap
               shape) self
    (setup-shape tire)
    (setup-shape wheel-rim)
    ;; update dependent geometry
    (with-slots ((hub-cap-base base) direction up (hub-cap-shape shape))
        hub-cap
      (with-slots (rim-offset (wheel-rim-shape shape)) wheel-rim
        (setf hub-cap-base (+ +origin+ (/ rim-offset 2)))
        (setf up +y-axis+)
        (setf direction (normalize rim-offset))
        (setup-shape hub-cap)))))

(defmethod render ((self bus-wheel))
  (with-slots (tire wheel-rim hub-cap) self
    (render tire)
    (render wheel-rim)
    (render hub-cap)
    (setf rendering
          (clive:separator
           (with-slots (rendering) tire rendering)
           (with-slots (rendering) wheel-rim rendering)
           (with-slots (rendering) hub-cap rendering)))))


;;
;; bus headlights class
;;

(defclass bus-headlight (bus-assembly)
  (;; specifications
   curvature-radius
   disk-radius
   direction
   up
   ;;
   glass-shape
   trim-shape))

(defmethod initialize-instance :after ((self bus-headlight) &key)
  (with-slots (curvature-radius disk-radius direction up)
      self
    (setf curvature-radius 0.3)
    (setf disk-radius 0.15)
    (setf direction +x-axis+)
    (setf up +y-axis+)))

(defmethod setup-shape ((self bus-headlight))
  (with-slots (parent curvature-radius disk-radius direction up
               glass-shape trim-shape glass-bounds trim-bounds)
      self
    (with-slots (length width height headlight-spacing headlight-height
                 wheel (bus-resolution resolution))
        parent
      (let* ((dish (make-instance 'dish
                                  :curvature-radius curvature-radius
                                  :disk-radius disk-radius
                                  :direction direction
                                  :up up
                                  :parent self))
             (dish-shape (setup-shape dish))
             (glass-offset #[0.01 0 0])
             (glass (make-instance 'dish
                                   :curvature-radius (* curvature-radius 0.6)
                                   :disk-radius (- disk-radius 0.03)
                                   :direction direction
                                   :up up
                                   :offset glass-offset
                                   :parent self))
             ;; increase the bus resolution by approximately the ratio
             ;; of the average bus dimension to the headlight radius
             (headlight-res
              (* bus-resolution
                 (/ (/ (+ length width height) 3)
                    disk-radius))))

        (setf glass-shape (setup-shape glass))
        (setf trim-shape (difference dish-shape glass-shape))
        (setf resolution headlight-res)
        (setf glass-bounds)))))

(defclass bus-headlights (bus-assembly)
  (left-light
   right-light))

(defmethod setup-shape ((self bus-headlight))
  (with-slots (parent curvature-radius disk-radius direction up
               shape)
      self
    (with-slots (length width headlight-spacing headlight-height
                 wheel)
        parent
      (let* ((offset-z (/ (- width headlight-spacing) 2))
             (wheel-height (with-slots (height) wheel height))
             (wheel-height/2 (/ wheel-height 2))
             ;; left
             (left-base
              #[length (- headlight-height wheel-height/2) offset-z])
             (left-dish (make-instance
                         'dish
                         :curvature-radius curvature-radius
                         :disk-radius disk-radius
                         :direction direction
                         :up up
                         :offset left-base
                         :parent self))
             (left-dish-shape (setup-shape left-dish))
             (glass-offset #[0.01 0 0])
             (left-glass (make-instance
                         'dish
                         :curvature-radius (* curvature-radius 0.6)
                         :disk-radius (- disk-radius 0.03)
                         :direction direction
                         :up up
                         :offset (+ left-base glass-offset)
                         :parent self))
             (left-glass-shape (setup-shape left-glass))
             (left-trim (difference left-dish-shape
                                    left-glass-shape))
             (left-light (union left-trim
                                left-glass-shape))
             ;; right
             (right-base #[length
                           (- headlight-height wheel-height/2)
                           (- width offset-z)])
             (right-dish (make-instance
                          'dish
                          :curvature-radius curvature-radius
                          :disk-radius disk-radius
                          :direction direction
                          :up up
                          :offset right-base
                          :parent self))
             (right-dish-shape (setup-shape right-dish))
             (right-glass (make-instance
                           'dish
                           :curvature-radius (* curvature-radius 0.6)
                           :disk-radius (- disk-radius 0.03)
                           :direction direction
                           :up up
                           :offset (+ right-base glass-offset)
                           :parent self))
             (right-glass-shape (setup-shape right-glass))
             (right-trim (difference right-dish-shape
                                     right-glass-shape))
             (right-light (union right-trim right-glass-shape)))
        (setf shape (union left-light right-light))))))


;;;
;;; bus body class
;;;

(defclass bus-body (bus-assembly)
  (;; specifications
   radiusing-value
   shell-thickness
   panel-gap
   ;; dimensions
   radiused-length
   radiused-width
   radiused-height
   top-radiused-length
   ;;
   front-rake-height-vector
   front-rake-start-point
   ;; parts
   solid
   front-solid
   rear-solid
   windshield-cutout
   rear-window-cutouts
   wheel-wells
   wheel-well-cutouts
   frame
   rear-frame
   front-frame
   fill
   rear-fill
   front-fill))

(defmethod initialize-instance :after ((self bus-body) &key)
  (with-slots (radiusing-value shell-thickness panel-gap) self
    (setf radiusing-value 0.20)
    (setf shell-thickness 0.05)
    (setf panel-gap 0.02)))

(defmethod calculate-dimensions ((self bus-body))
  (with-slots (parent
               radiusing-value
               radiused-length radiused-width radiused-height
               top-radiused-length) self
    (let ((radiusing-diameter (* radiusing-value 2)))
      (with-slots (length width height top-height) parent
        (setf radiused-length (- length radiusing-diameter))
        (with-slots (top-length) parent
          (setf top-radiused-length (- top-length radiusing-diameter)))
        (setf radiused-width (- width radiusing-diameter))
        (setf radiused-height (- height radiusing-diameter))))))

(defgeneric setup-frame (self))
(defmethod setup-frame ((self bus-body))
  (with-slots (parent
               radiusing-value
               radiused-length radiused-width radiused-height
               top-radiused-length
               front-frame
               rear-frame)
      self
    (with-slots (length height
                 belt-line-height
                 top-length)
        parent
      (let* ((radiusing-base
              #[radiusing-value radiusing-value radiusing-value])
             (frame-radiusing-sphere
              (sphere radiusing-value radiusing-base))
             (lower-frame-length-cylinder (cylinder-vec radiusing-value
                                                        #[radiused-length 0 0]
                                                        radiusing-base))
             (upper-frame-length-cylinder (cylinder-vec radiusing-value
                                                        #[top-radiused-length
                                                          0
                                                          0]
                                                        radiusing-base))
             (frame-width-cylinder (cylinder-vec radiusing-value
                                                 #[0 0 radiused-width]
                                                 radiusing-base))
             (rear-frame-height-cylinder (cylinder-vec radiusing-value
                                                       #[0 radiused-height 0]
                                                       radiusing-base))
             (front-frame-height (- radiused-height
                                    height
                                    (- (+ belt-line-height radiusing-value))))
             (front-frame-height-cylinder (cylinder-vec radiusing-value
                                                        #[0
                                                          front-frame-height
                                                          0]
                                                        radiusing-base))
             (frame-width (union frame-width-cylinder
                                 frame-radiusing-sphere
                                 (move frame-radiusing-sphere
                                       #[0 0 radiused-width])))
             ;; front rake
             (front-rake-start-pt #[(- length radiusing-value)
                                    belt-line-height
                                    radiusing-value])
             (front-rake-end-pt #[(- top-length radiusing-value)
                                     (- height radiusing-value)
                                     radiusing-value])
             (front-rake-height-vec (- front-rake-end-pt
                                       front-rake-start-pt))
             (front-rake (cylinder-vec radiusing-value
                                       front-rake-height-vec
                                       front-rake-start-pt)))
        (with-slots (rear-frame
                     front-frame
                     frame
                     front-rake-height-vector front-rake-start-point)
            self
          ;; setup rear frame
          (setf front-rake-height-vector front-rake-height-vec)
          (setf front-rake-start-point front-rake-start-pt)
          (setf rear-frame
                (union
                 ;; rear frame width
                 frame-width
                 (translate frame-width
                            #[0 radiused-height 0])
                 ;; rear frame height cylinders
                 rear-frame-height-cylinder
                 (translate rear-frame-height-cylinder
                            #[0 0 radiused-width])
                 ;; lower frame length cylinders
                 lower-frame-length-cylinder
                 (translate lower-frame-length-cylinder
                            #[0 0 radiused-width])
                 ;; upper frame length cylinders
                 (translate upper-frame-length-cylinder
                            #[0 radiused-height 0])
                 (translate upper-frame-length-cylinder
                            #[0 radiused-height radiused-width])))
          ;; setup front frame
          (setf front-frame
                (union
                 ;; front frame-width
                 (translate frame-width
                            #[radiused-length 0 0])
                 (translate frame-width
                            #[radiused-length front-frame-height 0])
                 (translate frame-width
                            #[top-radiused-length radiused-height 0])
                 ;; front frame height cylinders
                 (translate front-frame-height-cylinder
                            #[radiused-length 0 0])
                 (translate front-frame-height-cylinder
                            #[radiused-length 0 radiused-width])
                 ;; front rake pillars
                 front-rake
                 (translate front-rake
                            #[0 0 radiused-width])))
          ;;  add front and back frames to the main frame
          (setf frame (union front-frame rear-frame))
          (values rear-frame front-frame frame))))))

(defgeneric setup-fill (self))
(defmethod setup-fill ((self bus-body))
  (with-slots (parent radiusing-value radiused-width
               front-rake-height-vector front-rake-start-point)
    self
    (with-slots (length width height top-length belt-line-height) parent
      (let* ((body-rear-length (- top-length radiusing-value))
             (body-width (- width radiusing-value))
             (body-rear-height (- height radiusing-value))
             ;; rear boxes
             (rear-length-box (box #[0 radiusing-value radiusing-value]
                              #[top-length body-rear-height body-width]))
             (rear-width-box (box #[radiusing-value radiusing-value 0]
                                  #[body-rear-length body-rear-height width]))
             (rear-height-box (box #[radiusing-value 0 radiusing-value]
                                   #[body-rear-length height body-width]))
             ;; front boxes
             (front-start-length (- top-length radiusing-value))
             (front-length (- length radiusing-value))
             (front-height belt-line-height)
             (front-length-box
              (box
               #[front-start-length radiusing-value radiusing-value]
               #[length front-height body-width]))
             (front-width-box (box #[front-start-length radiusing-value 0]
                                   #[front-length front-height width]))
             (front-height-box (box #[front-start-length 0 radiusing-value]
                                    #[front-length
                                      (+ belt-line-height radiusing-value)
                                      body-width]))
             ;; rake
             (front-rake-width #[0 0 radiused-width])
             (front-rake-u (normalize front-rake-width))
             (front-rake-v (normalize front-rake-height-vector))
             (w (cross front-rake-v front-rake-u))
             (front-rake-length (* w radiusing-value))
             (front-rake-box (box-dim front-rake-start-point
                                      front-rake-width
                                      front-rake-height-vector
                                      front-rake-length))
             ;; wedge
             (wedge (wedge #[(- top-length radiusing-value)
                             belt-line-height
                             0]
                           #[(- length top-length) 0 0]
                           #[0
                             (- height (+ belt-line-height radiusing-value))
                             0]
                           #[0 0 width])))
        (with-slots (rear-fill front-fill fill) self
          ;; setup the fill combinations
          (setf rear-fill
                (union
                 rear-length-box
                 rear-width-box
                 rear-height-box))
          (setf front-fill
                (union
                 front-length-box
                 front-width-box
                 front-height-box
                 front-rake-box
                 wedge))
          (setf fill
                (union rear-fill front-fill))
          (values rear-fill front-fill fill))))))

(defgeneric setup-window-cutouts (self))
(defmethod setup-window-cutouts ((self bus-body))
  (with-slots (radiusing-value shell-thickness parent) self
    (with-slots (length width height top-length belt-line-height
                 front-window-width back-window-width)
        parent
      (let* ((windshield-rear-x (- top-length radiusing-value))
             (window-base-y (+ belt-line-height (/ radiusing-value 2)))
             (window-top-y (- height radiusing-value))
             ;; driver window box
             (front-window-rear-x (- windshield-rear-x
                                     front-window-width
                                     radiusing-value))
             (left-window-inner-z (+ shell-thickness radiusing-value))
             (driver-window-box
              (box #[front-window-rear-x window-base-y (- radiusing-value)]
                   #[(+ front-window-rear-x front-window-width)
                   window-top-y
                   left-window-inner-z]))
             ;; passenger window box
             (right-window-inner-z (- width shell-thickness radiusing-value))
             (passenger-window-box
              (box #[front-window-rear-x window-base-y right-window-inner-z]
                   #[(+ front-window-rear-x
                        front-window-width)
                   window-top-y
                   (+ width radiusing-value)]))
             ;;  back left window box
             (back-window-rear-x (* radiusing-value 2))
             (back-left-window-box
              (box #[back-window-rear-x window-base-y (- radiusing-value)]
                   #[(+ back-window-rear-x back-window-width)
                   window-top-y
                   left-window-inner-z]))
             ;; back right window box
             (back-right-window-box
              (box #[back-window-rear-x window-base-y right-window-inner-z]
                   #[(+ back-window-rear-x back-window-width)
                   window-top-y
                   (+ width radiusing-value)]))
             ;; middle left window box
             (middle-window-rear-x (+ back-window-rear-x
                                      back-window-width
                                      radiusing-value))
             (middle-left-window-box
              (box #[middle-window-rear-x window-base-y (- radiusing-value)]
                   #[(+ middle-window-rear-x
                        back-window-width)
                   window-top-y
                   left-window-inner-z]))
             ;; middle right window box
             (middle-right-window-box
              (box #[middle-window-rear-x window-base-y right-window-inner-z]
                   #[(+ middle-window-rear-x back-window-width)
                   window-top-y
                   (+ width radiusing-value)]))
             (rear-window-box
              (box #[(- radiusing-value) window-base-y radiusing-value]
                   #[(+ shell-thickness radiusing-value)
                   window-top-y
                   (- width radiusing-value)])))
        (with-slots (windshield-cutout rear-window-cutouts) self
          ;; windshield box
          (setf windshield-cutout
                (box #[windshield-rear-x window-base-y (- radiusing-value)]
                     #[(+ length radiusing-value)
                     window-top-y
                     (+ width radiusing-value)]))
          ;; rear window boxes
          (setf rear-window-cutouts
                (union rear-window-box
                       driver-window-box
                       passenger-window-box
                       middle-left-window-box
                       middle-right-window-box
                       back-left-window-box
                       back-right-window-box))
          (values windshield-cutout rear-window-cutouts))))))

(defgeneric setup-solid (self))
(defmethod setup-solid ((self bus-body))
  (with-slots (solid
               front-solid
               rear-solid
               frame fill
               front-frame front-fill
               rear-frame rear-fill) self
    (setf solid (union frame fill))
    (setf front-solid (union front-frame front-fill))
    (setf rear-solid (union rear-frame rear-fill))
    (values solid front-solid rear-solid)))

(defgeneric setup-wheel-wells (self))
(defmethod setup-wheel-wells ((self bus-body))
  (with-slots (parent
               shell-thickness
               wheel-well-cutouts
               wheel-wells
               (body-solid solid))
      self
    (with-slots (width height
                 wheel-to-body-clearance wheel-base
                 rear-wheel-x
                 wheel)
        parent
      (let* ((wheel-height (with-slots (height) wheel height))
             (wheel-well-radius (+ (/ wheel-height 2.0)
                                   wheel-to-body-clearance))
             (wheel-well-width (+ width (* wheel-to-body-clearance 2.0)))
             (wheel-well-width-each (/ wheel-height 2))
             ;; outer divider box
             (outer-well-box
              (box #[(- (+ wheel-well-radius wheel-to-body-clearance))
                     0
                     wheel-well-width-each]
                   #[(+ wheel-well-radius wheel-to-body-clearance)
                     (+ wheel-well-radius (* 2 wheel-to-body-clearance))
                     (- width wheel-well-width-each)]))
             ;; cylinder for outside the bus
             (outer-well
              (difference
               (cylinder-vec wheel-well-radius
                             #[0 0 wheel-well-width]
                             #[0 0 (- wheel-to-body-clearance)])
               outer-well-box))
             ;; inner divider box
             (inner-well-box
              (box #[(- (+ wheel-well-radius wheel-to-body-clearance))
                     0
                     (+ wheel-well-width-each shell-thickness)]
                   #[(+ wheel-well-radius wheel-to-body-clearance)
                     (+ wheel-well-radius (* 2 wheel-to-body-clearance))
                     (- width wheel-well-width-each shell-thickness)]))
             ;; slightly larger cylinder for the inside of the bus
             (inner-well
              (difference
               (cylinder-vec (+ wheel-well-radius shell-thickness)
                             #[0 0 wheel-well-width]
                             #[0 0 (- wheel-to-body-clearance)])
               inner-well-box)))
        (setf wheel-well-cutouts
              (union
               (translate outer-well #[rear-wheel-x 0 0])
               (translate outer-well #[(+ rear-wheel-x wheel-base) 0 0])))
        (setf wheel-wells
              (difference
               (intersection
                body-solid
                (union
                 (translate inner-well #[rear-wheel-x 0 0])
                 (translate inner-well #[(+ rear-wheel-x wheel-base) 0 0])))
               wheel-well-cutouts))
        (values wheel-well-cutouts wheel-wells)))))

(defmethod setup-shape ((self bus-body))
  (setup-frame self)
  (setup-fill self)
  (setup-window-cutouts self)
  (setup-solid self)
  (setup-wheel-wells self)
  (with-slots (parent
               shell-thickness
               solid windshield-cutout rear-window-cutouts
               rear-wheels-solid wheel-wells wheel-well-cutouts
               shape bounds)
      self
    (with-slots (length width height bus-resolution) parent
      (let* ((shell-thickness*2 (* shell-thickness 2))
             (offset-body
              (scale
               (move solid
                     #[shell-thickness shell-thickness shell-thickness])
               #[(/ (- length shell-thickness*2) length)
                 (/ (- width shell-thickness*2) width)
                 (/ (- height shell-thickness*2) height)]))
             (body-shell (difference solid offset-body)))
        (setf shape
              (union
               (difference body-shell
                           windshield-cutout
                           rear-window-cutouts
                           wheel-well-cutouts)
              wheel-wells))
        (setf bounds
              (region3 '((0 . length) (0 . height) (0 . width))))
        (setf resolution bus-resolution)))))


;;
;; bus class
;;

(defclass bus (bus-assembly)
  (;;
   ;; Specifications (parameters)
   ;;
   ;;   overall
   (width :initform 1.727 :initarg :width :reader get-width)
   (length :initform 4.505 :initarg :length :reader get-length)
   (height :initform 2.040 :initarg :height :reader get-height)
   (ride-height :initform 0.20 :initarg :ride-height)
   (belt-line-height :initform 1.143 :initarg :belt-line-height)
   ;;   wheel related
   (track :initform 1.374 :initarg :track)
   (wheel-base :initform 2.40 :initarg :wheel-base)
   (wheel-to-body-clearance :initform 0.10 :initarg :wheel-to-body-clearance)
   (rear-overhang :initform 0.686 :initarg :rear-overhang)
   (wheel-turn-angle :initform (deg-to-rad 15.0) :initarg :wheel-turn-angle)
   ;;   headlight related
   (headlight-height :initform 0.9 :initarg :headlight-height)
   (headlight-spacing :initform 0.9 :initarg :headlight-spacing)
   ;;   window related
   (front-window-width :initform 0.6 :initarg :front-window-width)
   (back-window-width :initform 1.2 :initarg :back-window-width)
   ;;
   ;; Calculated dimensions
   ;;
   ;;   children independent
   top-length
   ;;   children dependent
   rear-wheel-x
   ;; Parts
   ;;   prototypes (for dimensions)
   wheel
   bumper
   ;;   assemblies
   body
   wheels
   windows
   headlights
   bumpers))

(defgeneric setup-wheels (self))
(defgeneric setup-windows (self))
(defgeneric setup-bumpers (self))
(defgeneric setup-headlights (self))

(defmethod initialize-instance :after ((self bus) &key)
  ;; child independent calculated dimensions
  (with-slots (length top-length) self
    (setf top-length (- length (* length 0.0677))))
  ;; parts
  (with-slots (wheel bumper
               body wheels windows headlights bumpers)
      self
    ;; prototypes
    (setf wheel (make-instance 'bus-wheel :parent self))
    (setf bumper (make-instance 'bus-bumper :parent self))
    ;; assemblies
    (setf body (make-instance 'bus-body :parent self))
    ;; update part dimensions
    (calculate-dimensions wheel)
    (calculate-dimensions bumper)
    (calculate-dimensions body)
    ;; dependent calculated dimensions
    (with-slots (rear-overhang
                 wheel-to-body-clearance
                 rear-wheel-x)
        self
      (with-slots (height) wheel
        (let ((rear-wheel-x-value
               (+ rear-overhang (/ height 2) wheel-to-body-clearance)))
          (setf rear-wheel-x rear-wheel-x-value))))
    ;; setup the shapes for the parts (order dependent)
    ;; TODO: antenna, doors and rear lid, tail lights
    (setup-shape body)
    (setup-wheels self)
    (setup-windows self)
    (setup-headlights self)
    (setup-bumpers self)))

(defmethod setup-shape ((self bus))
  (with-slots (body wheels windows headlights bumpers
               shape)
      self
    (setf shape (union (with-slots (shape) body shape)
                       wheels
                       ;; windows
                       (with-slots (shape) headlights shape)
                       bumpers))))

(defmethod setup-wheels ((self bus))
  (with-slots (width track wheel-base
               wheel-turn-angle
               rear-wheel-x
               wheel
               wheels) self
    (setup-shape wheel)
    ;; wheels
    (let* ((front-wheel-x (+ rear-wheel-x wheel-base))
             (tire (with-slots (tire) wheel tire))
             (tire-width (with-slots (width) tire width))
             (left-wheel-z (/ (- width track tire-width) 2))
           (right-wheel-z (- width left-wheel-z))
           (left-wheel-rotation-angle (deg-to-rad 180))
           ;; rear left
             (wheel-shape (with-slots (shape) wheel shape))
           (rear-left-wheel (translate
                             (rotate-y
                              wheel-shape left-wheel-rotation-angle)
                             #[rear-wheel-x 0 left-wheel-z]))
           ;; front left
           (front-left-wheel (translate
                              (rotate-y
                               wheel-shape (- left-wheel-rotation-angle
                                              wheel-turn-angle))
                              #[front-wheel-x 0 left-wheel-z]))
           ;; rear right
           (rear-right-wheel (translate
                              wheel-shape
                              #[rear-wheel-x 0 right-wheel-z]))
           ;; front right
           (front-right-wheel (translate
                              (rotate-y
                               wheel-shape (- wheel-turn-angle))
                               #[front-wheel-x 0 right-wheel-z])))
      (setf wheels
            (union rear-left-wheel
                   front-left-wheel
                   rear-right-wheel
                   front-right-wheel)))))

(defmethod setup-windows ((self bus))
  (with-slots (body length width height windows) self
    (with-slots (front-solid rear-solid
                 windshield-cutout rear-window-cutouts)
        body
      (let* (;; windshield
             (window (make-instance 'bus-window
                                    :parent self
                                    :material *glass-material*))
             (glass-thickness
              (with-slots (glass-thickness) window glass-thickness))
             (glass-thickness*2 (* glass-thickness 2))
             (windshield-shell
              (intersection
               (difference
                front-solid
                (scale
                 (move front-solid
                       #[glass-thickness glass-thickness glass-thickness])
                 #[(/ (- length glass-thickness*2) length)
                 (/ (- height glass-thickness*2) height)
                 (/ (- width glass-thickness*2) width)]))
               windshield-cutout))
             ;; the rest of the windows
             (rear-window-shell
              (intersection
               (difference
                rear-solid
                (scale
                 (move rear-solid
                       #[glass-thickness glass-thickness glass-thickness])
                 #[(/ (- length glass-thickness*2) length)
                 (/ (- height glass-thickness*2) height)
                 (/ (- width glass-thickness*2) width)]))
               rear-window-cutouts)))
        (setf windows
              (union windshield-shell rear-window-shell))))))

(defmethod setup-bumpers ((self bus))
  (with-slots (bumper bumpers length) self
    (setup-shape bumper)
    (with-slots (shape body-gap flattening-offset radius) bumper
      (let* ((rear-bumper (translate
                           shape
                           #[(+ (- body-gap) flattening-offset)
                             (+ radius body-gap)
                             (+ (- body-gap) flattening-offset)]))
             (front-bumper (translate
                            (rotate-z shape (deg-to-rad 180))
                            #[(+ length body-gap (- flattening-offset))
                              (+ radius body-gap (- flattening-offset))
                              (+ (- body-gap) flattening-offset)])))
        (setf bumpers (union rear-bumper front-bumper))))))

(defmethod setup-headlights ((self bus))
  (with-slots (headlights) self
    (setf headlights (make-instance 'bus-headlights :parent self))
    (setup-shape headlights)))


 ;;                   ;;
;;                     ;;
;;   bus constructor   ;;
;;                     ;;
 ;;                   ;;

(defun bus ()
  (let ((bus (make-instance 'bus)))
    (setup-shape bus)
    bus))

(defun main ()
  (format t "*** Creating model...~%")
  (let* ((bus (bus))
         (rendered-bus (render-shape bus))
         ;; (bus-length (get-length bus))
         ;; (bus-width (get-width bus))
         ;; (bus-height (get-height bus))
         (scene
          (clive:separator
           ((clive:directional-light)
            (clive:perspective-camera)
            ;; (clive:separator
            ;; ((clive:material :diffuse-color (clive:color :b .8))
            ;;  (clive:light-model :model :light-model-base-color)
            ;;  (clive:draw-style :style :draw-style-lines
            ;;                    :line-width 2.5)
            ;;  (clive:translation :translation
            ;;                     (clive:vec3f :x (/ bus-length 2)
            ;;                                  :y (/ bus-height 2)
            ;;                                  :z (/ bus-width 2)))
            ;;  (clive:cube :width bus-length
            ;;              :height bus-height
            ;;              :depth bus-width)))
            ;;
            ;; for improved OpenGL rendering, let Clive know
            ;; that this is a watertight solid
            (clive:shape-hints :shape-type :shape-hints-solid
                               :vertex-ordering :shape-hints-counterclockwise)
            (rendered-bus)))))
    (format t "model created. ***~%")
    (clive-viewer:set-scene-graph-root scene)
    (clive-viewer:main :app-name "MagicBus" :window-title "Magic Bus")))
