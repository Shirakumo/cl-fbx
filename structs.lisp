#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fbx)

(defclass allocator ()
  ((memory-limit :initarg :memory-limit :initform 0 :accessor memory-limit)
   (allocation-limit :initarg :allocation-limit :initform 0 :accessor allocation-limit)
   (huge-threshold :initarg :huge-threshold :initform 0 :accessor huge-threshold)
   (max-chunk-size :initarg :max-chunk-size :initform 0 :accessor max-chunk-size)))

(defgeneric free (allocator))
(defgeneric allocate (allocator size))
(defgeneric reallocate (allocator old-pointer old-size new-size))
(defgeneric deallocate (allocator pointer size))

(defmethod set-string (target (string string))
  (setf (fbx:string-length target) (length string))
  (setf (fbx:string-data target) target))

(defmethod set-blob (target (vector vector))
  (setf (fbx:blob-size target) (length vector))
  (setf (fbx:blob-data target) (cffi:foreign-alloc :uint8 :count (length vector) :initial-contents vector)))

(defmethod set-callback (target (function function))
  (setf (fbx:progress-cb-fn target) (cffi:callback progress-cb))
  (setf (fbx:progress-cb-user target) target)
  (setf (global-pointer target) function))

(defmethod set-coordinate-axes (target (axes cons))
  (destructuring-bind (right up front) axes
    (setf (fbx:coordinate-axes-right target) right)
    (setf (fbx:coordinate-axes-up target) up)
    (setf (fbx:coordinate-axes-front target) front)))

(defmethod set-allocator (target (allocator allocator))
  (setf (fbx:allocator-alloc-fn target) (cffi:callback allocate-cb))
  (setf (fbx:allocator-realloc-fn target) (cffi:callback reallocate-cb))
  (setf (fbx:allocator-free-fn target) (cffi:callback free-cb))
  (setf (fbx:allocator-free-allocator-fn target) (cffi:callback free-allocator-cb))
  (setf (fbx:allocator-opts-memory-limit target) (memory-limit allocator))
  (setf (fbx:allocator-opts-allocation-limit target) (allocation-limit allocator))
  (setf (fbx:allocator-opts-huge-threshold target) (huge-threshold allocator))
  (setf (fbx:allocator-opts-max-chunk-size target) (max-chunk-size allocator)))

(defclass wrapper ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

(defclass foreign-vector (standard-object sequences:sequence)
  ((handle :initarg :handle :initform NIL :accessor handle)
   (lisp-type :initarg :lisp-type :initform NIL :accessor lisp-type)
   (foreign-type :initarg :foreign-type :initform NIL :accessor foreign-type)))

(defmethod sequences:elt ((vector foreign-vector) index)
  (make-instance (lisp-type vector) :handle (cffi:mem-aref (fbx:list-data (handle vector)) :pointer index)))

(defmethod (setf sequences:elt) ((value wrapper) (vector foreign-vector) index)
  (setf (cffi:mem-aref (fbx:list-data (handle vector)) :pointer index) (handle value))
  value)

(defmethod sequences:length ((vector foreign-vector))
  (fbx:list-count (handle vector)))

(defclass packed-foreign-vector (foreign-vector)
  ())

(defmethod sequences:elt ((vector packed-foreign-vector) index)
  (make-instance (lisp-type vector) :handle (cffi:mem-aptr (fbx:list-data (handle vector)) (foreign-type vector) index)))

(defmethod (setf sequences:elt) ((value wrapper) (vector packed-foreign-vector) index)
  (static-vectors:replace-foreign-memory
   (cffi:mem-aptr (fbx:list-data (handle vector)) (foreign-type vector) index)
   (handle value)
   (cffi:foreign-type-size (foreign-type vector)))
  value)

(defclass immediate-foreign-vector (foreign-vector)
  ())

(defmethod sequences:elt ((vector packed-foreign-vector) index)
  (cffi:mem-aref (fbx:list-data (handle vector)) (foreign-type vector) index))

(defmethod (setf sequences:elt) (value (vector packed-foreign-vector) index)
  (setf (cffi:mem-aref (fbx:list-data (handle vector)) (foreign-type vector) index) value)
  value)

(defun make-foreign-vector (ptr lisp-type foreign-type)
  (case foreign-type
    ((:bool :double :float :char :pointer :size :int :int64 :uint64 :int32 :uint32 :int8 :uint8)
     (make-instance 'immediate-foreign-vector :handle ptr :foreign-type foreign-type))
    (T
     (case lisp-type
       ((vec2 vec3 vec4 fbx-string dom-value prop connection uv-set color-set edge face mesh-material
              face-group subdivision-weight-range subdivision-weight line-segment lod-level skin-vertex
              skin-weight blend-keyframe cache-frame cache-channel material-texture texture-layer
              shader-texture-input texture-file shader-prop-binding anim-layer-desc prop-override
              anim-prop keyframe constraint-target bone-pose name-element warning)
        (make-instance 'foreign-vector :handle ptr :foreign-type foreign-type :lisp-type lisp-type))
       (T
        (make-instance 'foreign-vector :handle ptr :foreign-type foreign-type :lisp-type lisp-type))))))

(defun compile-slot-accessor (type class slot-name slot-type slot-opts)
  (let ((accessor (or (getf slot-opts :accessor) (intern (string slot-name))))
        (slot-fun (find-symbol (format NIL "~a-~a" type slot-name) (symbol-package type)))
        (lisp-type (cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                          'foreign-vector)
                         ((and (listp slot-type) (eql :struct (first slot-type)))
                          (intern (string (second slot-type))))
                         ((and (listp slot-type) (eql :array (first slot-type)))
                          'vector)
                         (T (case slot-type
                              (:pointer (or (getf slot-opts :lisp-type)
                                            (progn (warn "~&; No canonical type known for ~a.~a" type slot-name)
                                                   T)))
                              (:float 'single-float)
                              (:double 'double-float)
                              (:char 'cl:character)
                              ((:size :int :int64 :uint64 :int32 :uint32 :int8 :uint8) 'integer)
                              (:bool T)
                              (T T))))))
    `(progn (defmethod ,accessor ((wrapper ,class))
              ,(cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                      `(make-foreign-vector (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)
                                            ',(or (getf slot-opts :lisp-type)
                                                  (when (getf slot-opts :foreign-type)
                                                    (intern (string (getf slot-opts :foreign-type))))
                                                  (progn (warn "~&; No canonical type known for ~a.~a" type slot-name)
                                                         'wrapper))
                                            ',(or (getf slot-opts :foreign-type)
                                                  (when (getf slot-opts :lisp-type)
                                                    (intern (string (getf slot-opts :lisp-type)) (symbol-package type)))
                                                  :pointer)))
                     ((and (listp slot-type) (eql :struct (first slot-type)))
                      `(make-instance ',lisp-type :handle (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)))
                     ((and (eql :pointer slot-type) (not (eql T lisp-type)))
                      `(let ((value (,slot-fun (handle wrapper))))
                         (unless (cffi:null-pointer-p value)
                           (make-instance ',lisp-type :handle value))))
                     (T
                      `(,slot-fun (handle wrapper)))))
            
            (defmethod (setf ,accessor) ((value ,lisp-type) (wrapper ,class))
              ,(cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                      `(error "Impl"))
                     ((and (listp slot-type) (eql :struct (first slot-type)))
                      `(static-vectors:replace-foreign-memory
                        (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)
                        (handle value)
                        ,(cffi:foreign-type-size slot-type)))
                     ((eql :pointer slot-type)
                      `(setf (,slot-fun (handle wrapper)) (handle value)))
                     (T
                      `(setf (,slot-fun (handle wrapper)) value)))
              value)

            ,@(when (eql :pointer slot-type)
                `((defmethod (setf ,accessor) ((value null) (wrapper ,class))
                    (setf (,slot-fun (handle wrapper)) (cffi:null-pointer))
                    value))))))

(defmacro define-struct-accessors (type class &body slot-options)
  (let ((slots (cffi:foreign-slot-names `(:struct ,type))))
    `(progn 
       ,@(loop for slot-name in slots
               for slot-type = (cffi:foreign-slot-type `(:struct ,type) slot-name)
               for slot-opts = (rest (assoc slot-name slot-options :test #'string=))
               unless (getf slot-opts :omit)
               collect (compile-slot-accessor type class slot-name slot-type slot-opts)))))

(defmacro define-struct-wrappers (&body structs)
  (let ((structs (loop for (type . rest) in structs
                       collect (destructuring-bind (type &optional (class (intern (string type)))) (if (listp type) type (list type))
                                 (list* type class rest)))))
    `(progn
       ,@(loop for (type class) in structs
               collect `(defclass ,class (wrapper) ()))
       ,@(loop for (type class . slots) in structs
               collect `(define-struct-accessors ,type ,class ,@slots)))))

(define-struct-wrappers
  ((fbx:string fbx-string)
   (length :accessor size))
  (fbx:blob
   (data :lisp-type T))
  (fbx:vec2)
  (fbx:vec3)
  (fbx:vec4)
  (fbx:quat)
  (fbx:transform)
  (fbx:matrix)
  (fbx:dom-value
   (type :accessor value-type))
  ;;(fbx:list)
  (fbx:dom-node
   (children :lisp-type dom-node)
   (values :lisp-type dom-value))
  (fbx:prop
   (type :accessor prop-type))
  (fbx:props
   (props :lisp-type prop)
   (defaults :lisp-type props))
  (fbx:element
   (type :accessor element-type)
   (dom-node :lisp-type dom-node)
   (scene :lisp-type scene)
   (instances :lisp-type node)
   (connections-src :lisp-type connection)
   (connections-dst :lisp-type connection))
  (fbx:connection
   (src :lisp-type element)
   (dst :lisp-type element))
  (fbx:unknown
   (type :accessor unknown-type :lisp-type fbx-string))
  (fbx:node
   (parent :lisp-type node)
   (children :lisp-type node)
   (mesh :lisp-type mesh)
   (light :lisp-type light)
   (camera :lisp-type camera)
   (bone :lisp-type bone)
   (attrib :lisp-type element)
   (geometry-transform-helper :lisp-type node)
   (all-attribs :lisp-type element)
   (materials :lisp-type material))
  (fbx:vertex-attrib
   (values :lisp-type T)
   (indices :foreign-type :uint32))
  (fbx:vertex-real
   (values :lisp-type single-float :foreign-type :float)
   (indices :foreign-type :uint32))
  (fbx:vertex-vec2
   (values :lisp-type vec2)
   (indices :foreign-type :uint32))
  (fbx:vertex-vec3
   (values :lisp-type vec3)
   (indices :foreign-type :uint32))
  (fbx:vertex-vec4
   (values :lisp-type vec4)
   (indices :foreign-type :uint32))
  (fbx:uv-set)
  (fbx:color-set)
  (fbx:edge)
  (fbx:face)
  (fbx:mesh-material
   (material :lisp-type material)
   (face-indices :foreign-type :uint32))
  (fbx:face-group
   (face-indices :foreign-type :uint32))
  (fbx:subdivision-weight-range)
  (fbx:subdivision-weight)
  (fbx:subdivision-result
   (source-vertex-ranges :lisp-type subdivision-weight-range)
   (source-vertex-weights :lisp-type subdivision-weight)
   (skin-cluster-ranges :lisp-type subdivision-weight-range)
   (skin-cluster-weights :lisp-type subdivision-weight))
  (fbx:mesh
   (subdivision-result :lisp-type subdivision-result)
   (faces :lisp-type face)
   (face-smoothing :foreign-type :bool)
   (face-material :foreign-type :uint32)
   (face-group :foreign-type :uint32)
   (face-hole :foreign-type :bool)
   (edges :lisp-type edge)
   (edge-smoothing :foreign-type :bool)
   (edge-crease :foreign-type :float)
   (edge-visibility :foreign-type :bool)
   (vertex-indices :foreign-type :uint32)
   (vertices :lisp-type vec3)
   (vertex-first-index :foreign-type :uint32)
   (uv-sets :lisp-type uv-set)
   (color-sets :lisp-type color-set)
   (materials :lisp-type material)
   (face-groups :lisp-type face-group)
   (skin-deformers :lisp-type skin-deformer)
   (blend-deformers :lisp-type blend-deformer)
   (cache-deformers :lisp-type cache-deformer)
   (all-deformers :lisp-type element))
  (fbx:light
   (type :accessor light-type))
  (fbx:coordinate-axes)
  (fbx:camera)
  (fbx:bone)
  (fbx:empty)
  (fbx:line-segment)
  (fbx:line-curve
   (control-points :lisp-type vec3)
   (point-indices :foreign-type :uint32)
   (segments :lisp-type line-segment))
  (fbx:nurbs-basis
   (knot-vector :foreign-type :float)
   (spans :foreign-type :float))
  (fbx:nurbs-curve
   (control-points :lisp-type vec4))
  (fbx:nurbs-surface
   (material :lisp-type material)
   (control-points :lisp-type vec4))
  (fbx:nurbs-trim-surface)
  (fbx:nurbs-trim-boundary)
  (fbx:procedural-geometry)
  (fbx:stereo-camera
   (left :lisp-type camera)
   (right :lisp-type camera))
  (fbx:camera-switcher)
  (fbx:marker
   (type :accessor marker-type))
  (fbx:lod-level)
  (fbx:lod-group
   (lod-levels :lisp-type lod-level))
  (fbx:skin-vertex)
  (fbx:skin-weight)
  (fbx:skin-deformer
   (clusters :lisp-type skin-cluster)
   (vertices :lisp-type skin-vertex)
   (weights :lisp-type skin-weight)
   (dq-vertices :foreign-type :uint32)
   (dq-weights :foreign-type :float))
  (fbx:skin-cluster
   (bone-node :lisp-type node)
   (vertices :foreign-type :uint32)
   (weights :foreign-type :float))
  (fbx:blend-deformer
   (channels :lisp-type blend-channel))
  (fbx:blend-keyframe
   (shape :lisp-type blend-shape))
  (fbx:blend-channel
   (keyframes :lisp-type blend-keyframe))
  (fbx:blend-shape
   (offset-vertices :foreign-type :uint32)
   (position-offsets :lisp-type vec3)
   (normal-offsets :lisp-type vec3))
  (fbx:cache-frame)
  (fbx:cache-channel
   (frames :lisp-type cache-frame))
  (fbx:geometry-cache
   (channels :lisp-type cache-channel)
   (frames :lisp-type cache-frame)
   (extra-info :lisp-type fbx-string))
  (fbx:cache-deformer
   (file :lisp-type cache-file)
   (external-cache :lisp-type geometry-cache)
   (external-channel :lisp-type cache-channel))
  (fbx:cache-file
   (format :accessor file-format)
   (external-cache :lisp-type geometry-cache))
  (fbx:material-map
   (texture :lisp-type texture))
  (fbx:material-feature-info)
  (fbx:material-texture
   (texture :lisp-type texture))
  (fbx:material-fbx-maps)
  (fbx:material-pbr-maps)
  (fbx:material-features)
  (fbx:material
   (shader :lisp-type shader)
   (textures :lisp-type material-texture))
  (fbx:texture-layer
   (texture :lisp-type texture))
  (fbx:shader-texture-input
   (texture :lisp-type texture)
   (prop :lisp-type prop)
   (texture-prop :lisp-type prop)
   (texture-enabled-prop :lisp-type prop))
  (fbx:shader-texture
   (type :accessor shader-texture-type)
   (inputs :lisp-type shader-texture-input)
   (main-texture :lisp-type texture))
  (fbx:texture-file)
  (fbx:texture
   (type :accessor texture-type)
   (video :lisp-type video)
   (shader :lisp-type shader-texture)
   (layers :lisp-type texture-layer)
   (file-textures :lisp-type texture))
  (fbx:video)
  (fbx:shader
   (type :accessor shader-type)
   (bindings :lisp-type shader-binding))
  (fbx:shader-prop-binding)
  (fbx:shader-binding
   (prop-bindings :lisp-type shader-prop-binding))
  (fbx:anim-layer-desc
   (layer :lisp-type anim-layer))
  (fbx:prop-override)
  (fbx:anim
   (layers :lisp-type anim-layer-desc)
   (prop-overrides :lisp-type prop-override))
  (fbx:anim-stack
   (layers :lisp-type anim-layer))
  (fbx:anim-prop
   (anim-value :lisp-type anim-value)
   (element :lisp-type element))
  (fbx:anim-layer
   (anim-values :lisp-type anim-value)
   (anim-props :lisp-type anim-prop))
  (fbx:anim-value
   (curves :lisp-type anim-curve))
  (fbx:tangent)
  (fbx:keyframe)
  (fbx:anim-curve
   (keyframes :lisp-type keyframe))
  (fbx:display-layer
   (nodes :lisp-type node))
  (fbx:selection-set
   (nodes :lisp-type node))
  (fbx:selection-node
   (target-node :lisp-type node)
   (target-mesh :lisp-type mesh)
   (vertices :foreign-type :uint32)
   (edges :foreign-type :uint32)
   (faces :foreign-type :uint32))
  (fbx:character)
  (fbx:constraint-target
   (node :lisp-type node))
  (fbx:constraint
   (type :accessor constraint-type)
   (node :lisp-type node)
   (aim-up-node :lisp-type node)
   (ik-effector :lisp-type node)
   (ik-end-node :lisp-type node)
   (targets :lisp-type constraint-target))
  (fbx:bone-pose
   (bone-node :lisp-type node))
  (fbx:pose
   (bone-poses :lisp-type bone-pose))
  (fbx:metadata-object)
  (fbx:name-element
   (type :accessor element-type)
   (element :lisp-type element))
  (fbx:application)
  ;;(fbx:warning)
  (fbx:metadata
   (warnings :lisp-type fbx-warning))
  (fbx:scene-settings)
  (fbx:scene
   (root-node :lisp-type node)
   (dom-root :lisp-type node)
   (unknowns :lisp-type unknown)
   (nodes :lisp-type node)
   (meshes :lisp-type mesh)
   (lights :lisp-type light)
   (cameras :lisp-type camera)
   (bones :lisp-type bone)
   (empties :lisp-type empty)
   (line-curves :lisp-type line-curve)
   (nurbs-curves :lisp-type nurbs-curve)
   (nurbs-surfaces :lisp-type nurbs-surface)
   (nurbs-trim-surfaces :lisp-type nurbs-trim-surface)
   (nurbs-trim-boundaries :lisp-type nurbs-trim-boundary)
   (procedural-geometries :lisp-type procedural-geometry)
   (stereo-cameras :lisp-type stereo-camera)
   (camera-switchers :lisp-type camera-switcher)
   (markers :lisp-type marker)
   (lod-groups :lisp-type lod-group)
   (skin-deformers :lisp-type skin-deformer)
   (skin-clusters :lisp-type skin-cluster)
   (blend-deformers :lisp-type blend-deformer)
   (blend-channels :lisp-type blend-channel)
   (blend-shapes :lisp-type blend-shape)
   (cache-deformers :lisp-type cache-deformer)
   (cache-files :lisp-type cache-file)
   (materials :lisp-type material)
   (textures :lisp-type texture)
   (videos :lisp-type video)
   (shaders :lisp-type shader)
   (shader-bindings :lisp-type shader-binding)
   (anim-stacks :lisp-type anim-stack)
   (anim-layers :lisp-type anim-layer)
   (anim-values :lisp-type anim-value)
   (anim-curves :lisp-type anim-curve)
   (display-layers :lisp-type display-layer)
   (selection-sets :lisp-type selection-set)
   (selection-nodes :lisp-type selection-node)
   (characters :lisp-type character)
   (constraints :lisp-type constraint)
   (poses :lisp-type pose)
   (metadata-objects :lisp-type metadata-object)
   (texture-files :lisp-type texture-file)
   (elements :lisp-type element)
   (connections-src :lisp-type connection)
   (connections-dst :lisp-type connection)
   (elements-by-name :lisp-type name-element))
  (fbx:curve-point)
  (fbx:surface-point)
  (fbx:topo-edge)
  (fbx:vertex-stream
   (data :lisp-type T))
  ;;(fbx:allocator)
  ;;(fbx:allocator-opts)
  ;;(fbx:stream)
  ;;(fbx:open-file-info)
  ;;(fbx:open-file-cb)
  ;;(fbx:close-memory-cb)
  ;;(fbx:open-memory-opts)
  ;;(fbx:error-frame)
  ;;(fbx:error)
  ;;(fbx:progress)
  ;;(fbx:progress-cb)
  (fbx:inflate-input
   (progress-cb :omit T)
   (data :lisp-type T)
   (buffer :lisp-type T)
   (read-fn :lisp-type T)
   (read-user :lisp-type T))
  (fbx:inflate-retain)
  ;;(fbx:load-opts)
  ;;(fbx:evaluate-opts)
  ;;(fbx:tessellate-curve-opts)
  ;;(fbx:tessellate-surface-opts)
  ;;(fbx:subdivide-opts)
  ;;(fbx:geometry-cache-opts)
  ;;(fbx:geometry-cache-data-opts)
  ;;(fbx:panic)
  )
