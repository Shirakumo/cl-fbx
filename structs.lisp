#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fbx)

(defclass foreign-vector (standard-object sequences:sequence)
  ((handle :initarg :handle :initform NIL :accessor handle)
   (element-type :initarg :element-type :initform NIL :accessor element-type)))

(defclass wrapper ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

(defun compile-slot-accessor (type class slot-name slot-type slot-opts)
  (let ((accessor (or (getf slot-opts :accessor) (intern (string slot-name))))
        (lisp-type (cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                          'foreign-vector)
                         ((and (listp slot-type) (eql :struct (first slot-type)))
                          (intern (string (second slot-type))))
                         ((and (listp slot-type) (eql :array (first slot-type)))
                          'vector)
                         (T (case slot-type
                              (:pointer (or (getf slot-opts :lisp-type)
                                            (progn (alexandria:simple-style-warning "No canonical type known for ~s" slot-name)
                                                   T)))
                              (:float 'single-float)
                              (:double 'double-float)
                              (:char 'cl:character)
                              ((:size :int :int64 :uint64 :int32 :uint32 :int8 :uint8) 'integer)
                              (:bool T)
                              (T T))))))
    `(progn (defmethod ,accessor ((wrapper ,class))
              ,(cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                      `(make-instance 'foreign-vector :handle (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)
                                                      :element-type ,(or (getf slot-opts :element-type)
                                                                         (progn (alexandria:simple-style-warning "No canonical element-type known for ~s" slot-name)
                                                                                :pointer))))
                     ((and (listp slot-type) (eql :struct (first slot-type)))
                      `(make-instance ',lisp-type :handle (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)))
                     ((and (eql :pointer slot-type) (not (eql T lisp-type)))
                      `(make-instance ',lisp-type :handle (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)))
                     (T
                      `(,(find-symbol (format NIL "~a-~a" type slot-name) (symbol-package type)) (handle wrapper)))))
            
            (defmethod (setf ,accessor) ((value ,lisp-type) (wrapper ,class))
              ,(cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                      `(error "Impl"))
                     ((and (listp slot-type) (eql :struct (first slot-type)))
                      `(static-vectors:replace-foreign-memory
                        (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)
                        (handle value)
                        ,(cffi:foreign-type-size slot-type)))
                     ((eql :pointer slot-type)
                      `(setf (,(find-symbol (format NIL "~a-~a" type slot-name) (symbol-package type)) (handle wrapper)) (handle value)))
                     (T
                      `(setf (,(find-symbol (format NIL "~a-~a" type slot-name) (symbol-package type)) (handle wrapper)) value)))
              value))))

(defmacro define-struct-wrapper (type &body slot-options)
  (let ((slots (cffi:foreign-slot-names `(:struct ,type))))
    (destructuring-bind (type &optional (class (intern (string type)))) (if (listp type) type (list type))
      `(progn 
         (defclass ,class (wrapper) ())
         
         ,@(loop for slot-name in slots
                 for slot-type = (cffi:foreign-slot-type `(:struct ,type) slot-name)
                 for slot-opts = (rest (assoc slot-name slot-options :test #'string=))
                 collect (compile-slot-accessor type class slot-name slot-type slot-opts))))))

(define-struct-wrapper fbx:string)
(define-struct-wrapper fbx:blob)
(define-struct-wrapper fbx:vec2)
(define-struct-wrapper fbx:vec3)
(define-struct-wrapper fbx:vec4)
(define-struct-wrapper fbx:quat)
;(define-struct-wrapper fbx:transform)
(define-struct-wrapper fbx:matrix)
(define-struct-wrapper fbx:dom-value
  (type :accessor value-type))
;(define-struct-wrapper fbx:list)
(define-struct-wrapper fbx:dom-node)
(define-struct-wrapper fbx:prop
  (type :accessor prop-type))
(define-struct-wrapper fbx:props)
(define-struct-wrapper fbx:connection)
(define-struct-wrapper fbx:element
  (type :accessor element-type)
  (dom-node :lisp-type dom-node)
  (scene :lisp-type scene))
(define-struct-wrapper fbx:unknown
  (type :accessor unknown-type :lisp-type fbx-string))
(define-struct-wrapper fbx:node)
(define-struct-wrapper fbx:vertex-attrib)
(define-struct-wrapper fbx:vertex-real)
(define-struct-wrapper fbx:vertex-vec2)
(define-struct-wrapper fbx:vertex-vec3)
(define-struct-wrapper fbx:vertex-vec4)
(define-struct-wrapper fbx:uv-set)
(define-struct-wrapper fbx:color-set)
(define-struct-wrapper fbx:edge)
(define-struct-wrapper fbx:face)
(define-struct-wrapper fbx:mesh-material
  (material :lisp-type material))
(define-struct-wrapper fbx:face-group)
(define-struct-wrapper fbx:subdivision-weight-range)
(define-struct-wrapper fbx:subdivision-weight)
(define-struct-wrapper fbx:subdivision-result)
(define-struct-wrapper fbx:mesh
  (subdivision-result :lisp-type subdivision-result))
(define-struct-wrapper fbx:light
  (type :accessor light-type))
(define-struct-wrapper fbx:coordinate-axes)
(define-struct-wrapper fbx:camera)
(define-struct-wrapper fbx:bone)
(define-struct-wrapper fbx:empty)
(define-struct-wrapper fbx:line-segment)
(define-struct-wrapper fbx:line-curve)
(define-struct-wrapper fbx:nurbs-basis)
(define-struct-wrapper fbx:nurbs-curve)
(define-struct-wrapper fbx:nurbs-surface
  (material :lisp-type material))
(define-struct-wrapper fbx:nurbs-trim-surface)
(define-struct-wrapper fbx:nurbs-trim-boundary)
(define-struct-wrapper fbx:procedural-geometry)
(define-struct-wrapper fbx:stereo-camera
  (left :lisp-type camera)
  (right :lisp-type camera))
(define-struct-wrapper fbx:camera-switcher)
(define-struct-wrapper fbx:marker
  (type :accessor marker-type))
(define-struct-wrapper fbx:lod-level)
(define-struct-wrapper fbx:lod-group)
(define-struct-wrapper fbx:skin-vertex)
(define-struct-wrapper fbx:skin-weight)
(define-struct-wrapper fbx:skin-deformer)
(define-struct-wrapper fbx:skin-cluster
  (bone-node :lisp-type node))
(define-struct-wrapper fbx:blend-deformer)
(define-struct-wrapper fbx:blend-keyframe
  (shape :lisp-type blend-shape))
(define-struct-wrapper fbx:blend-channel)
(define-struct-wrapper fbx:blend-shape)
(define-struct-wrapper fbx:cache-frame)
(define-struct-wrapper fbx:cache-channel)
(define-struct-wrapper fbx:geometry-cache)
(define-struct-wrapper fbx:cache-deformer
  (file :lisp-type cache-file)
  (external-cache :lisp-type geometry-cache)
  (external-channel :lisp-type cache-channel))
(define-struct-wrapper fbx:cache-file
  (format :accessor file-format)
  (external-cache :lisp-type geometry-cache))
(define-struct-wrapper fbx:material-map
  (texture :lisp-type texture))
(define-struct-wrapper fbx:material-feature-info)
(define-struct-wrapper fbx:material-texture
  (texture :lisp-type texture))
(define-struct-wrapper fbx:material-fbx-maps)
(define-struct-wrapper fbx:material-pbr-maps)
(define-struct-wrapper fbx:material-features)
(define-struct-wrapper fbx:material
  (shader :lisp-type shader))
(define-struct-wrapper fbx:texture-layer
  (texture :lisp-type texture))
(define-struct-wrapper fbx:shader-texture-input
  (texture :lisp-type texture)
  (prop :lisp-type prop)
  (texture-prop :lisp-type prop)
  (texture-enabled-prop :lisp-type prop))
(define-struct-wrapper fbx:shader-texture
  (type :accessor shader-texture-type)
  (main-texture :lisp-type texture))
(define-struct-wrapper fbx:texture-file)
(define-struct-wrapper fbx:texture
  (type :accessor texture-type)
  (video :lisp-type video)
  (shader :lisp-type shader-texture))
(define-struct-wrapper fbx:video)
(define-struct-wrapper fbx:shader
  (type :accessor shader-type))
(define-struct-wrapper fbx:shader-prop-binding)
(define-struct-wrapper fbx:shader-binding)
(define-struct-wrapper fbx:anim-layer-desc
  (layer :lisp-type anim-layer))
(define-struct-wrapper fbx:prop-override)
(define-struct-wrapper fbx:anim)
(define-struct-wrapper fbx:anim-stack)
(define-struct-wrapper fbx:anim-prop
  (anim-value :lisp-type anim-value))
(define-struct-wrapper fbx:anim-layer)
(define-struct-wrapper fbx:anim-value
  (curves :lisp-type anim-curve))
(define-struct-wrapper fbx:tangent)
(define-struct-wrapper fbx:keyframe)
(define-struct-wrapper fbx:anim-curve)
(define-struct-wrapper fbx:display-layer)
(define-struct-wrapper fbx:selection-set)
(define-struct-wrapper fbx:selection-node
  (target-node :lisp-type node)
  (target-mesh :lisp-type mesh))
(define-struct-wrapper fbx:character)
(define-struct-wrapper fbx:constraint-target
  (node :lisp-type node))
(define-struct-wrapper fbx:constraint
  (type :accessor constraint-type)
  (node :lisp-type node)
  (aim-up-node :lisp-type node)
  (ik-effector :lisp-type node)
  (ik-end-node :lisp-type node))
(define-struct-wrapper fbx:bone-pose
  (bone-node :lisp-type node))
(define-struct-wrapper fbx:pose)
(define-struct-wrapper fbx:metadata-object)
(define-struct-wrapper fbx:name-element
  (type :accessor element-type))
(define-struct-wrapper fbx:application)
;(define-struct-wrapper fbx:warning)
(define-struct-wrapper fbx:metadata)
(define-struct-wrapper fbx:scene-settings)
(define-struct-wrapper fbx:scene
  (root-node :lisp-type node)
  (dom-root :lisp-type node))
(define-struct-wrapper fbx:curve-point)
(define-struct-wrapper fbx:surface-point)
(define-struct-wrapper fbx:topo-edge)
(define-struct-wrapper fbx:vertex-stream)
;(define-struct-wrapper fbx:allocator)
;(define-struct-wrapper fbx:allocator-opts)
;(define-struct-wrapper fbx:stream)
;(define-struct-wrapper fbx:open-file-info)
;(define-struct-wrapper fbx:open-file-cb)
;(define-struct-wrapper fbx:close-memory-cb)
;(define-struct-wrapper fbx:open-memory-opts)
;(define-struct-wrapper fbx:error-frame)
;(define-struct-wrapper fbx:error)
;(define-struct-wrapper fbx:progress)
;(define-struct-wrapper fbx:progress-cb)
(define-struct-wrapper fbx:inflate-input)
(define-struct-wrapper fbx:inflate-retain)
;(define-struct-wrapper fbx:load-opts)
;(define-struct-wrapper fbx:evaluate-opts)
;(define-struct-wrapper fbx:tessellate-curve-opts)
;(define-struct-wrapper fbx:tessellate-surface-opts)
;(define-struct-wrapper fbx:subdivide-opts)
;(define-struct-wrapper fbx:geometry-cache-opts)
;(define-struct-wrapper fbx:geometry-cache-data-opts)
;(define-struct-wrapper fbx:panic)

