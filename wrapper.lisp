#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fbx)

(defvar *global-pointer-table* (make-hash-table :test 'eql))

(defun global-pointer (ptr)
  (gethash ptr *global-pointer-table*))

(defun (setf global-pointer) (value ptr)
  (if value
      (setf (gethash ptr *global-pointer-table*) value)
      (remhash ptr *global-pointer-table*))
  vlaue)

(defmacro with-ptr-resolve ((value ptr) &body body)
  `(let ((,value (global-pointer ,ptr)))
     (when ,value
       ,@body)))

;; FIXME: clean up global pointer table for struct members somehow.

(defclass allocator ()
  ((memory-limit :initarg :memory-limit :initform 0 :accessor memory-limit)
   (allocation-limit :initarg :allocation-limit :initform 0 :accessor allocation-limit)
   (huge-threshold :initarg :huge-threshold :initform 0 :accessor huge-threshold)
   (max-chunk-size :initarg :max-chunk-size :initform 0 :accessor max-chunk-size)))

(defgeneric free (allocator))
(defgeneric allocate (allocator size))
(defgeneric reallocate (allocator old-pointer old-size new-size))
(defgeneric deallocate (allocator pointer size))

(defun update (wrapper &rest args)
  (loop for slot-name in (cffi:foreign-slot-names (foreign-type wrapper))
        for lisp-name = (intern (string slot-name) #.*package*)
        for value = (getf args (intern (string slot-name) "KEYWORD") #1='#:no-value)
        do (unless (eq value #1#)
             (funcall (fdefinition (list 'setf slot-name)) value wrapper)))
  wrapper)

(defmacro with-freeing (bindings &body body)
  (let ((gensyms (loop for (name) in bindings collect (gensym (string name)))))
    `(let ,(loop for gens in gensyms
                 for binding in bindings
                 collect `(,gens ,(second binding)))
       (unwind-protect (let ,(loop for gens in gensyms
                                   for (name) in bindings
                                   collect `(,gens ,name))
                         ,@body)
         ,@(loop for gens in gensyms
                 collect `(free ,gens))))))

(defclass wrapper ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

(defmethod initialize-instance :after ((wrapper wrapper) &rest args &key handle &allow-other-keys)
  (unless handle
    (static-vectors:fill-foreign-memory (handle wrapper) (cffi:foreign-type-size (foreign-type wrapper)) 0))
  (apply #'update wrapper args))

(defmethod reinitialize-instance :after ((wrapper wrapper) &rest args &key &allow-other-keys)
  (apply #'update wrapper args))

(defgeneric foreign-type (wrapper))

(defmethod free ((wrapper wrapper))
  (when (handle wrapper)
    (setf (global-pointer (handle wrapper)) NIL)
    (cffi:foreign-free (handle wrapper))
    (setf (handle wrapper) NIL)))

(defmethod close ((wrapper wrapper) &key abort)
  (declare (ignore abort))
  (free wrapper))

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

(defmethod data ((vector foreign-vector))
  (fbx:list-data (handle vector)))

(defmethod free ((vector foreign-vector))
  (when (handle vector)
    (unless (cffi:null-pointer-p (fbx:list-data (handle vector)))
      (cffi:foreign-free (fbx:list-data (handle vector))))
    (setf (fbx:list-data (handle vector)) (cffi:null-pointer))
    (setf (fbx:list-count (handle vector)) 0)
    (setf (handle vector) NIL)))

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

(defun wrap-foreign-vector (ptr lisp-type foreign-type)
  (case foreign-type
    ((:bool :double :float :char :pointer :size :int :int64 :uint64 :int32 :uint32 :int8 :uint8)
     (make-instance 'immediate-foreign-vector :handle ptr :foreign-type foreign-type))
    (T
     (case lisp-type
       ((vec2 vec3 vec4 fbx-string dom-value prop connection uv-set color-set edge face mesh-material
              face-group subdivision-weight-range subdivision-weight line-segment lod-level skin-vertex
              skin-weight blend-keyframe cache-frame cache-channel material-texture texture-layer
              shader-texture-input texture-file shader-prop-binding anim-layer-desc prop-override
              anim-prop keyframe constraint-target bone-pose name-element warning topo-edge)
        (make-instance 'packed-foreign-vector :handle ptr :foreign-type foreign-type :lisp-type lisp-type))
       (T
        (make-instance 'foreign-vector :handle ptr :foreign-type foreign-type :lisp-type lisp-type))))))

(defun make-foreign-vector (foreign-type size)
  (let ((list (cffi:foreign-alloc '(:struct fbx:list))))
    (setf (fbx:list-count list) size)
    (case foreign-type
      ((:bool :double :float :char :pointer :size :int :int64 :uint64 :int32 :uint32 :int8 :uint8)
       (setf (fbx:list-data list) (cffi:foreign-alloc foreign-type :count size))
       (make-instance 'immediate-foreign-vector :handle list :foreign-type foreign-type))
      (T
       (let ((lisp-type (intern (string foreign-type) #.*package*)))
         (case lisp-type
           ((vec2 vec3 vec4 fbx-string dom-value prop connection uv-set color-set edge face mesh-material
                  face-group subdivision-weight-range subdivision-weight line-segment lod-level skin-vertex
                  skin-weight blend-keyframe cache-frame cache-channel material-texture texture-layer
                  shader-texture-input texture-file shader-prop-binding anim-layer-desc prop-override
                  anim-prop keyframe constraint-target bone-pose name-element warning topo-edge)
            (setf (fbx:list-data list) (cffi:foreign-alloc foreign-type :count size))
            (make-instance 'packed-foreign-vector :handle list :foreign-type foreign-type :lisp-type lisp-type))
           (T
            (setf (fbx:list-data list) (cffi:foreign-alloc :pointer :count size :initial-element (cffi:null-pointer)))
            (make-instance 'foreign-vector :handle list :foreign-type foreign-type :lisp-type lisp-type))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-lisp-type (slot-type slot-opts)
    (cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
           'foreign-vector)
          ((and (listp slot-type) (eql :struct (first slot-type)))
           (or (getf slot-opts :lisp-type)
               (intern (string (second slot-type)))))
          ((and (listp slot-type) (eql :array (first slot-type)))
           'vector)
          (T (case slot-type
               (:pointer (or (getf slot-opts :lisp-type) 'cffi:foreign-pointer))
               (:float 'single-float)
               (:double 'double-float)
               (:char 'cl:character)
               ((:size :int :int64 :uint64 :int32 :uint32 :int8 :uint8) 'integer)
               (:bool 'boolean)
               (T T)))))
  
  (defun compile-slot-accessor (type class slot-name slot-type slot-opts)
    (let* ((accessor (or (getf slot-opts :accessor) (intern (string slot-name))))
           (slot-fun (find-symbol (format NIL "~a-~a" type slot-name) (symbol-package type)))
           (lisp-type (to-lisp-type slot-type slot-opts))
           (value-class (case lisp-type
                          ((boolean cffi:foreign-pointer) T)
                          (T lisp-type))))
      `(progn
         (export '(,accessor))
         (defmethod ,accessor ((wrapper ,class))
           ,(cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                   `(wrap-foreign-vector (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)
                                         ',(or (getf slot-opts :lisp-type)
                                               (when (getf slot-opts :foreign-type)
                                                 (intern (string (getf slot-opts :foreign-type))))
                                               (progn (warn "~&; No canonical type known for ~a.~a" type slot-name)
                                                      'wrapper))
                                         ',(or (getf slot-opts :foreign-type)
                                               (when (getf slot-opts :lisp-type)
                                                 (intern (string (getf slot-opts :lisp-type)) (symbol-package type)))
                                               :pointer)))
                  ((and (listp slot-type) (eql 'fbx:string (first slot-type)))
                   `(fbx:string-data (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)))
                  ((and (listp slot-type) (eql :struct (first slot-type)))
                   `(make-instance ',lisp-type :handle (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)))
                  ((and (eql :pointer slot-type) (not (eql T lisp-type)))
                   `(let ((value (,slot-fun (handle wrapper))))
                      (unless (cffi:null-pointer-p value)
                        (make-instance ',lisp-type :handle value))))
                  (T
                   `(,slot-fun (handle wrapper)))))
         
         (defmethod (setf ,accessor) ((value ,value-class) (wrapper ,class))
           ,(cond ((and (listp slot-type) (eql 'fbx:list (second slot-type)))
                   `(error "Impl"))
                  ((and (listp slot-type) (eql 'fbx:string (first slot-type)))
                   `(setf (fbx:string-data (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)) value
                          (fbx:string-length (cffi:foreign-slot-pointer (handle wrapper) '(:struct ,type) ',slot-name)) (length value)))
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

         ,@(case slot-type
             (:pointer
              `((defmethod (setf ,accessor) ((value null) (wrapper ,class))
                  (setf (,slot-fun (handle wrapper)) (cffi:null-pointer))
                  value)))
             (fbx:blob
              `((defmethod (setf ,accessor) ((value vector) (wrapper ,class))
                  (setf (data (,accessor wrapper)) value)))))))))

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
                       collect (destructuring-bind (type &optional (class (intern (string type))) &rest copts) (if (listp type) type (list type))
                                 (list* type class copts rest))))
        (inv-map (make-hash-table :test 'eql)))
    `(progn
       ,@(loop for (type class opts . slot-opts) in structs
               for slot-names = (cffi:foreign-slot-names `(:struct ,type))
               for method-names = (loop for slot in slot-names
                                        for opts = (rest (assoc slot slot-opts :test #'string=))
                                        collect (or (getf opts :accessor) (intern (string slot))))
               do (loop for slot in slot-names
                        for method in method-names
                        for opts = (rest (assoc slot slot-opts :test #'string=))
                        unless (getf opts :omit)
                        do (push (list slot class type opts) (gethash method inv-map)))
               collect `(export '(,class))
               collect `(defclass ,class (,@(getf opts :direct-superclasses) wrapper)
                          ((handle :initform (cffi:foreign-alloc '(:struct ,type))))
                          (:documentation ,(format NIL "Representation of a ~s.~%~{~%See ~s~}" type method-names))))
       ,@(loop for method being the hash-keys of inv-map using (hash-value classes)
               for slot = (first (first classes))
               collect `(progn (defgeneric ,method (wrapper)
                                 (:documentation ,(format NIL "Access the ~s slot~%~{~%~{For ~s returns a ~s~@[ (~s)~]~}~}~%~{~%See ~a (type)~}" slot
                                                          (loop for (slot-name class type slot-opts) in classes
                                                                for slot-type = (cffi:foreign-slot-type type slot-name)
                                                                for lisp-type = (to-lisp-type slot-type slot-opts)
                                                                collect (list class (if (eql T lisp-type)
                                                                                        slot-type lisp-type)
                                                                              (when (eql lisp-type 'foreign-vector)
                                                                                (or (getf slot-opts :lisp-type)
                                                                                    (intern (string (getf slot-opts :foreign-type)))))))
                                                          (mapcar #'second classes))))))
       ,@(loop for (type class copts . slots) in structs
               collect `(defmethod foreign-type ((wrapper ,class)) '(:struct ,type))
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
  ((fbx:vertex-real vertex-real :direct-superclasses (sequences:sequence))
   (values :lisp-type single-float :foreign-type :float)
   (indices :foreign-type :uint32))
  ((fbx:vertex-vec2 vertex-vec2 :direct-superclasses (sequences:sequence))
   (values :lisp-type vec2)
   (indices :foreign-type :uint32))
  ((fbx:vertex-vec3 vertex-vec3 :direct-superclasses (sequences:sequence))
   (values :lisp-type vec3)
   (indices :foreign-type :uint32))
  ((fbx:vertex-vec4 vertex-vec4 :direct-superclasses (sequences:sequence))
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
  (fbx:load-opts
   (temp-allocator :lisp-type allocator)
   (result-allocator :lisp-type allocator)
   (progress-cb :omit T)
   (open-file-cb :omit T))
  (fbx:evaluate-opts
   (temp-allocator :lisp-type allocator)
   (result-allocator :lisp-type allocator)
   (open-file-cb :omit T))
  (fbx:tessellate-curve-opts
   (temp-allocator :lisp-type allocator)
   (result-allocator :lisp-type allocator))
  (fbx:tessellate-surface-opts
   (temp-allocator :lisp-type allocator)
   (result-allocator :lisp-type allocator))
  (fbx:subdivide-opts
   (temp-allocator :lisp-type allocator)
   (result-allocator :lisp-type allocator))
  (fbx:geometry-cache-opts
   (temp-allocator :lisp-type allocator)
   (result-allocator :lisp-type allocator)
   (open-file-cb :omit T))
  (fbx:geometry-cache-data-opts
   (open-file-cb :omit T))
  ;;(fbx:panic)
  )

(defmethod free ((scene scene))
  (when (handle scene)
    (fbx:free-scene (handle scene))
    (setf (handle scene) NIL)))

(defmethod free ((curve line-curve))
  (when (handle curve)
    (fbx:free-line-curve (handle curve))
    (setf (handle curve) NIL)))

(defmethod free ((mesh mesh))
  (when (handle mesh)
    (fbx:free-mesh (handle mesh))
    (setf (handle mesh) NIL)))

(defmethod free ((geometry-cache geometry-cache))
  (when (handle geometry-cache)
    (fbx:free-geometry-cache (handle geometry-cache))
    (setf (handle geometry-cache) NIL)))

(defmethod (setf data) ((vector vector) (blob blob))
  (let* ((handle (handle blob))
         (pointer (cffi:foreign-funcall "realloc" :pointer (fbx:blob-data handle) :size (length vector))))
    (cffi:lisp-array-to-foreign vector pointer (list :array :char (length vector)))
    (setf (fbx:blob-data handle) pointer)
    (setf (fbx:blob-size handle) (length vector))
    vector))

(cffi:defcallback progress-cb fbx:progress-result ((user :pointer) (progress :pointer))
  (with-ptr-resolve (fun user)
    (funcall fun (fbx:progress-bytes-read progress) (fbx:progress-bytes-total progress))))

(defmethod (setf progress-cb) ((function function) (wrapper wrapper))
  (let ((handle (handle wrapper)))
    (setf (fbx:progress-cb-fn handle) (cffi:callback progress-cb))
    (setf (fbx:progress-cb-user handle) handle)
    (setf (global-pointer handle) function)
    function))

(cffi:defcallback open-file-cb :bool ((user :pointer) (stream :pointer) (path :string) (length :size) (info :pointer))
  (with-ptr-resolve (fun user)
    (funcall fun stream path (fbx:open-file-info-type info)
             (cffi:foreign-slot-pointer info '(:struct fbx:open-file-info) 'fbx:temp-allocator))))

(defmethod (setf open-file-cb) ((function function) (wrapper wrapper))
  (let ((handle (handle wrapper)))
    (setf (fbx:open-file-cb-fn handle) (cffi:callback open-file-cb))
    (setf (fbx:open-file-cb-user handle) handle)
    (setf (global-pointer handle) function)
    function))

(cffi:defcallback close-memory-cb :void ((user :pointer) (data :pointer) (size :size))
  (with-ptr-resolve (fun user)
    (funcall fun data size)))

(defmethod (setf close-memory-cb) ((function function) (wrapper wrapper))
  (let ((handle (handle wrapper)))
    (setf (fbx:close-memory-cb-fn handle) (cffi:callback close-memory-cb))
    (setf (fbx:close-memory-cb-user handle) handle)
    (setf (global-pointer handle) function)
    function))

(cffi:defcallback alloc-cb :pointer ((user :pointer) (size :size))
  (with-ptr-resolve (allocator user)
    (allocate allocator size)))

(cffi:defcallback realloc-cb :pointer ((user :pointer) (old-ptr :pointer) (old-size :size) (new-size :size))
  (with-ptr-resolve (allocator user)
    (reallocate allocator old-ptr old-size new-size)))

(cffi:defcallback free-cb :void ((user :pointer) (ptr :pointer) (size :size))
  (with-ptr-resolve (allocator user)
    (deallocate allocator ptr size)))

(cffi:defcallback free-allocator-cb :void ((user :pointer))
  (with-ptr-resolve (allocator user)
    (free allocator)))

(defmethod (setf temp-allocator) ((allocator allocator) (wrapper wrapper))
  (let ((handle (handle wrapper)))
    (setf (fbx:allocator-alloc-fn handle) (cffi:callback allocate-cb))
    (setf (fbx:allocator-realloc-fn handle) (cffi:callback reallocate-cb))
    (setf (fbx:allocator-free-fn handle) (cffi:callback free-cb))
    (setf (fbx:allocator-free-allocator-fn handle) (cffi:callback free-allocator-cb))
    (setf (fbx:allocator-opts-memory-limit handle) (memory-limit allocator))
    (setf (fbx:allocator-opts-allocation-limit handle) (allocation-limit allocator))
    (setf (fbx:allocator-opts-huge-threshold handle) (huge-threshold allocator))
    (setf (fbx:allocator-opts-max-chunk-size handle) (max-chunk-size allocator))
    allocator))

(defmethod (setf result-allocator) ((allocator allocator) (wrapper wrapper))
  (let ((handle (handle wrapper)))
    (setf (fbx:allocator-alloc-fn handle) (cffi:callback allocate-cb))
    (setf (fbx:allocator-realloc-fn handle) (cffi:callback reallocate-cb))
    (setf (fbx:allocator-free-fn handle) (cffi:callback free-cb))
    (setf (fbx:allocator-free-allocator-fn handle) (cffi:callback free-allocator-cb))
    (setf (fbx:allocator-opts-memory-limit handle) (memory-limit allocator))
    (setf (fbx:allocator-opts-allocation-limit handle) (allocation-limit allocator))
    (setf (fbx:allocator-opts-huge-threshold handle) (huge-threshold allocator))
    (setf (fbx:allocator-opts-max-chunk-size handle) (max-chunk-size allocator))
    allocator))

(defgeneric find (name container &key default type))
(defmethod find ((name string) (props props) &key default type)
  (let ((handle (handle props)))
    (cffi:with-foreign-string ((name length) name)
      (cond ((and default (not type))
             (setf type (type-of default)))
            ((not type)
             (setf type 'prop)))
      (ecase type
        (prop
         (let ((ptr (fbx:find-prop handle name length)))
           (if (cffi:null-pointer-p ptr)
               default
               (make-instance 'prop :handle ptr))))
        (vec3
         (cffi:with-foreign-objects ((ret '(:struct fbx:vec3)))
           (unless default
             (setf (fbx:vec3-x ret) float-features:single-float-nan)
             (setf (fbx:vec3-y ret) float-features:single-float-nan)
             (setf (fbx:vec3-z ret) float-features:single-float-nan))
           (fbx:find-vec3 ret props name length (if default (handle default) ret))
           (unless (float-features:float-nan-p (fbx:vec3-x ret))
             (let ((vec (make-array 3 :element-type 'single-float)))
               (setf (aref vec 0) (fbx:vec3-x ret))
               (setf (aref vec 1) (fbx:vec3-y ret))
               (setf (aref vec 2) (fbx:vec3-z ret))
               vec))))
        (string
         (cffi:with-foreign-objects ((str '(:struct fbx:string)))
           (cond (default
                  (setf (fbx:string-data str) default)
                  (setf (fbx:string-length str) (length default)))
                 (T
                  (setf (fbx:string-data str) (cffi:null-pointer))
                  (setf (fbx:string-length str) 0)))
           (fbx:find-string str props name length str)
           (unless (cffi:null-pointer-p (cffi:foreign-slot-pointer str '(:struct fbx:string) 'fbx:data))
             (fbx:string-data str))))
        (blob
         (cffi:with-foreign-objects ((ret '(:struct fbx:blob)))
           (setf (fbx:blob-size ret) 0)
           (setf (fbx:blob-data ret) (cffi:null-pointer))
           (fbx:find-blob ret props name length (if default (handle default) ret))
           (unless (cffi:null-pointer-p (fbx:blob-data ret))
             (cffi:foreign-array-to-lisp (fbx:blob-data ret) (list :array :uint8 (fbx:blob-size ret))))))
        (integer
         (fbx:find-int handle name length (or default 0)))
        (single-float
         (fbx:find-real handle name length (or default 0.0)))
        (boolean
         (fbx:find-bool handle name length (or default NIL)))))))

(defmethod find ((name string) (scene scene) &key default type)
  (let ((handle (handle scene)))
    (cffi:with-foreign-string ((name length) name)
      (cond ((and default (not type))
             (setf type (type-of default)))
            ((not type)
             (setf type 'node)))
      (case type
        (node
         (let ((ptr (fbx:find-node handle name length)))
           (if (cffi:null-pointer-p ptr)
               default
               (make-instance 'node :handle ptr))))
        (anim-stack
         (let ((ptr (fbx:find-anim-stack handle name length)))
           (if (cffi:null-pointer-p ptr)
               default
               (make-instance 'anim-stack :handle ptr))))
        (material
         (let ((ptr (fbx:find-material handle name length)))
           (if (cffi:null-pointer-p ptr)
               default
               (make-instance 'material :handle ptr))))
        (T
         (let ((ptr (fbx:find-element handle (intern (string type) "KEYWORD") name length)))
           (if (cffi:null-pointer-p ptr)
               default
               (make-instance type :handle ptr))))))))

(defmethod find ((name string) (material material) &key default type)
  (declare (ignore type))
  (cffi:with-foreign-string ((name length) name)
    (let ((ptr (fbx:find-prop-texture (handle material) name length)))
      (if (cffi:null-pointer-p ptr)
          default
          (make-instance 'texture :handle ptr)))))

(defmethod find ((name string) (shader shader) &key default type)
  (declare (ignore type))
  (cffi:with-foreign-string ((name length) name)
    (let ((ptr (fbx:find-shader-texture-input (handle material) name length)))
      (if (cffi:null-pointer-p ptr)
          default
          (make-instance 'shader-texture-input :handle ptr)))))

(defmethod find ((name string) (layer anim-layer) &key default type element)
  (declare (ignore type))
  (cffi:with-foreign-string ((name length) name)
    (let ((ptr (fbx:find-anim-prop (handle layer) (handle element) name length)))
      (if (cffi:null-pointer-p ptr)
          default
          (make-instance 'anim-prop :handle ptr)))))

(defmethod find ((name string) (node dom-node) &key default type)
  (declare (ignore type))
  (cffi:with-foreign-string ((name length) name)
    (let ((ptr (fbx:dom-find (handle node) name length)))
      (if (cffi:null-pointer-p ptr)
          default
          (make-instance 'dom-node :handle ptr)))))

(defmethod find ((element element) (layer anim-layer) &key default type)
  (declare (ignore type default))
  (cffi:with-foreign-objects ((list '(:struct fbx:list)))
    (fbx:find-anim-props list (handle layer) (handle element))
    (loop for i from 0 below (fbx:list-count list)
          collect (make-instance 'anim-prop :handle (cffi:mem-aref (fbx:list-data list) :pointer i)))))

(defmethod get-element ((element element) (prop prop) type)
  (make-instance 'element :handle (fbx:get-prop-element (handle element) (handle prop) type)))

(defmethod evaluate ((curve anim-curve) tt &key default)
  (fbx:evaluate-curve (handle curve) (float tt 0d0) (or default 0f0)))

(defmethod evaluate ((value anim-value) tt &key type)
  (case type
    ((real float single-float)
     (fbx:evaluate-anim-value-real (handle value) (float tt 0d0)))
    (vec2
     (cffi:with-foreign-objects ((ret '(:struct fbx:vec2)))
       (fbx:evaluate-anim-value-vec2 ret (handle value) (float tt 0d0))
       (let ((vec (make-array 2 :element-type 'single-float)))
         (setf (aref vec 0) (fbx:vec2-x ret))
         (setf (aref vec 1) (fbx:vec2-y ret))
         vec)))
    (vec3
     (cffi:with-foreign-objects ((ret '(:struct fbx:vec3)))
       (fbx:evaluate-anim-value-vec3 ret (handle value) (float tt 0d0))
       (let ((vec (make-array 3 :element-type 'single-float)))
         (setf (aref vec 0) (fbx:vec3-x ret))
         (setf (aref vec 1) (fbx:vec3-y ret))
         (setf (aref vec 2) (fbx:vec3-z ret))
         vec)))))

(defmethod evaluate ((animation anim) tt &key channel type element node name target)
  (case (or type 'real)
    (real
     (fbx:evaluate-blend-weight (handle animation) (handle channel) (float tt 0d0)))
    (prop
     (cffi:with-foreign-string ((name length) name)
       (let ((prop (or target (make-instance 'prop))))
         (fbx:evaluate-prop (handle prop) (handle animation) (handle element) name length (float tt 0d0))
         prop)))
    (transform
     (let ((transform (or target (make-instance 'transform))))
       (fbx:evaluate-transform (handle transform) (handle animation) (handle node)( float tt 0d0))
       transform))))

(defmethod evaluate ((scene scene) tt &rest options &key animation &allow-other-keys)
  (cffi:with-foreign-objects ((error '(:struct fbx:error)))
    (let* ((opts (apply #'make-instance 'evaluate-opts options))
           (handle (fbx:evaluate-scene (handle scene) (handle animation) (float tt 0d0) opts error)))
      (unwind-protect
           (check-error error)
        (free opts))
      (make-instance 'scene :handdle handle))))

(defmethod evaluate ((basis nurbs-basis) u &key weights derivatives)
  (let ((count (order basis)))
    (unless weights (setf weights (make-array count :element-type 'single-float)))
    (unless derivatives (setf derivatives (make-array count :element-type 'single-float)))
    (cffi:with-pointer-to-vector-data (wptr weights)
      (cffi:with-pointer-to-vector-data (dptr derivatives)
        (cl:values (fbx:evaluate-nurbs-basis (handle basis) (float u 0f0) wptr (length weights) dptr (length derivatives))
                   weights
                   derivatives)))))

(defmethod evaluate ((curve nurbs-curve) u &key point)
  (unless point (setf point (make-instance 'curve-point)))
  (fbx:evaluate-nurbs-curve (handle point) (handle curve) (float u 0f0))
  point)

(defmethod evaluate ((curve nurbs-surface) uv &key point)
  (unless point (setf point (make-instance 'surface-point)))
  (destructuring-bind (u . v) uv
    (fbx:evaluate-nurbs-surface (handle point) (handle curve) (float u 0f0) (float v 0f0)))
  point)

(defmethod add-offsets ((shape blend-shape) vertices weight)
  (cffi:with-pointer-to-vector-data (vptr vertices)
    (fbx:add-blend-shape-vertex-offsets (handle shape) vptr (length vertices) (float weight 0f0))))

(defmethod add-offsets ((shape blend-deformer) vertices weight)
  (cffi:with-pointer-to-vector-data (vptr vertices)
    (fbx:add-blend-vertex-offsets (handle shape) vptr (length vertices) (float weight 0f0))))

(defmethod tessellate ((curve nurbs-curve) &rest options)
  (cffi:with-foreign-objects ((error '(:struct fbx:error)))
    (let* ((opts (apply #'make-instance 'tessellate-curve-opts options))
           (handle (fbx:tessellate-nurbs-curve (handle curve) (handle opts) error)))
      (unwind-protect (check-error error)
        (free opts))
      (make-instance 'line-curve :handle handle))))

(defmethod tessellate ((surface nurbs-surface) &rest options)
  (cffi:with-foreign-objects ((error '(:struct fbx:error)))
    (let* ((opts (apply #'make-instance 'tessellate-surface-opts options))
           (handle (fbx:tessellate-nurbs-surface (handle surface) (handle opts) error)))
      (unwind-protect (check-error error)
        (free opts))
      (make-instance 'mesh :handle handle))))

(defmethod compute-topology ((mesh mesh) &key topo)
  (if topo
      (cffi:with-foreign-objects ((panic '(:struct fbx:panic)))
        (fbx:compute-topology panic (handle mesh) (handle topo) (num-indices mesh))
        (check-panic panic)
        topo)
      (let ((topo (make-foreign-vector '(:struct fbx:topo-edge) (num-indices mesh))))
        (cffi:with-foreign-objects ((panic '(:struct fbx:panic)))
          (fbx:compute-topology panic (handle mesh) (handle topo) (num-indices mesh))
          (when (fbx:panic-did-panic panic)
            (free topo)
            (fbx-panic panic))
          topo))))

(defmethod next-vertex ((vec foreign-vector) index)
  (cffi:with-foreign-objects ((panic '(:struct fbx:panic)))
    (let ((res (fbx:topo-next-vertex-edge panic (data vec) (length vec) index)))
      (check-panic panic)
      res)))

(defmethod prev-vertex ((vec foreign-vector) index)
  (cffi:with-foreign-objects ((panic '(:struct fbx:panic)))
    (let ((res (fbx:topo-prev-vertex-edge panic (data vec) (length vec) index)))
      (check-panic panic)
      res)))

(defmethod generate-normal-mapping ((mesh mesh) (topo foreign-vector) &key assume-smooth normal-indices)
  (cffi:with-foreign-objects ((panic '(:struct fbx:panic)))
    (unless normal-indices
      (setf normal-indices (make-array (num-indices mesh) :element-type '(unsigned-byte 32))))
    (cffi:with-pointer-to-vector-data (ptr normal-indices)
      (cl:values (fbx:generate-normal-mapping panic (handle mesh) (data topo) (length topo) ptr (length normal-indices) assume-smooth)
                 normal-indices))))

(defmethod compute-normals ((mesh mesh) positions normal-indices &key normals)
  (cffi:with-foreign-objects ((panic '(:struct fbx:panic)))
    (unless normals
      (setf normals (make-array (* 3 (length normal-indices)) :element-type 'single-float)))
    (cffi:with-pointer-to-vector-data (nptr normals)
      (cffi:with-pointer-to-vector-data (pptr positions)
        (cl:values (fbx:compute-normals panic (handle mesh) pptr (data normal-indices) (length normal-indices) nptr (truncate (length normals) 3))
                   normals)))))

(defmethod subdivide-mesh ((mesh mesh) level &rest opts)
  (cffi:with-foreign-objects ((error '(:struct fbx:error)))
    (let* ((opts (apply #'make-instance 'subdivide-opts opts))
           (ptr (fbx:subdivide-mesh (handle mesh) level opts error)))
      (unwind-protect (check-error error)
        (free opts))
      (make-instance 'mesh :handle ptr))))

(defmethod inflate ((target vector) (input inflate-input))
  (cffi:with-pointer-to-vector-data (ptr target)
    (cffi:with-foreign-objects ((retain '(:struct fbx:inflate-retain)))
      (setf (fbx:inflate-retain-initialized retain) NIL)
      (let ((decompressed (fbx:inflate ptr (length target) (handle input) retain)))
        (if (< decompressed 0)
            (inflate-error decompressed)
            decompressed)))))

(defmethod read-cache ((cache cache-frame) vector &rest opts &key (type 'real) &allow-other-keys)
  (let ((opts (apply #'make-instance 'geometry-cache-data-opts opts)))
    (cffi:with-pointer-to-vector-data (ptr vector)
      (unwind-protect
           (ecase type
             (vec3
              (fbx:read-geometry-cache-vec3 (handle cache) ptr (truncate (length vector) 3) opts))
             ((single-float float real)
              (fbx:read-geometry-cache-real (handle cache) ptr (length vector) opts)))
        (free opts)))))

(defmethod sample-cache ((cache cache-frame) vector time &rest opts &key (type 'real) &allow-other-keys)
  (let ((opts (apply #'make-instance 'geometry-cache-data-opts opts)))
    (cffi:with-pointer-to-vector-data (ptr vector)
      (unwind-protect
           (ecase type
             (vec3
              (fbx:sample-geometry-cache-vec3 (handle cache) (float time 0d0) ptr (truncate (length vector) 3) opts))
             ((single-float float real)
              (fbx:sample-geometry-cache-real (handle cache) (float time 0d0) ptr (length vector) opts)))
        (free opts)))))

(defmacro define-vertex-wrap (type wrap-size)
  (let ((element-type (ecase wrap-size
                        (1 :float)
                        (2 '(:struct fbx:vec2))
                        (3 '(:struct fbx:vec3))
                        (4 '(:struct fbx:vec4)))))
    `(progn
       (defmethod sequences:length ((vertex ,type))
         (fbx:list-count (cffi:foreign-slot-pointer (handle vertex) '(:struct fbx:vertex-real) 'fbx::values)))

       (defmethod sequences:elt ((vertex ,type) index)
         (let* ((handle (handle vertex))
                (idx (cffi:mem-aref (fbx:list-data (cffi:foreign-slot-pointer handle '(:struct fbx:vertex-real) 'fbx::indices)) :uint32 index))
                (ptr (cffi:mem-aptr (fbx:list-data (cffi:foreign-slot-pointer handle '(:struct fbx:vertex-real) 'fbx::values)) ',element-type idx)))
           ,(case wrap-size
              (1 `(cffi:mem-ref ptr ,element-type))
              (T `(cffi:foreign-array-to-lisp ptr '(:array :float ,wrap-size)
                                              :element-type 'single-float))))))))

(define-vertex-wrap vertex-real 1)
(define-vertex-wrap vertex-vec2 2)
(define-vertex-wrap vertex-vec3 3)
(define-vertex-wrap vertex-vec4 4)

(defmethod normal-matrix ((node node) &optional data)
  (unless data (setf data (make-array 16 :element-type 'single-float)))
  (cffi:with-pointer-to-vector-data (ptr data)
    (fbx:get-compatible-matrix-for-normals ptr (handle node))
    ptr))

(defmethod skin-vertex-matrix ((skin skin-deformer) index matrix)
  (cffi:with-pointer-to-vector-data (ptr matrix)
    (fbx:get-skin-vertex-matrix ptr (handle skin) index ptr)
    matrix))

(defmethod blend-vertex-offset ((shape blend-shape) index vertex)
  (cffi:with-pointer-to-vector-data (ptr vertex)
    (fbx:get-blend-shape-vertex-offset ptr (handle shape) index)
    vertex))

(defmethod weighted-face-normal ((face face) vertices)
  (cffi:with-pointer-to-vector-data (ptr vertices)
    (cffi:with-foreign-objects ((vec '(:struct fbx:vec3)))
      (fbx:get-weighted-face-normal vec ptr (handle face))
      (cffi:foreign-array-to-lisp ptr '(:array :float 3) :element-type 'single-float))))

(defmethod triangulate-face ((mesh mesh) (face face) (indices null))
  (let ((indices (make-array (fbx:get-triangulate-face-num-indices (handle face))
                             :element-type '(unsigned-byte 32))))
    (triangulate-face mesh face indices)))

(defmethod triangulate-face ((mesh mesh) (face face) (indices vector))
  (cffi:with-pointer-to-vector-data (ptr indices)
    (cl:values (fbx:triangulate-face ptr (length indices) (handle mesh) (handle face))
               indices)))
