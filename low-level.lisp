(in-package #:org.shirakumo.fraf.fbx.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library libfbx
  (:darwin (:or #+X86 "libfbx-mac-i686.dylib"
                #+X86-64 "libfbx-mac-amd64.dylib"
                #+ARM64 "libfbx-mac-arm64.dylib"))
  (:unix (:or #+X86 "libfbx-lin-i686.so"
              #+X86-64 "libfbx-lin-amd64.so"))
  (:windows (:or #+X86 "libfbx-win-i686.dll"
                 #+X86-64 "libfbx-win-amd64.dll"))
  (T (:or (:default "libufbx") (:default "ufbx"))))

(alexandria:define-constant FBX-LCL-TRANSLATION "Lcl Translation" :test #'string=)
(alexandria:define-constant FBX-LCL-ROTATION "Lcl Rotation" :test #'string=)
(alexandria:define-constant FBX-LCL-SCALING "Lcl Scaling" :test #'string=)
(alexandria:define-constant FBX-ROTATION-ORDER "RotationOrder" :test #'string=)
(alexandria:define-constant FBX-SCALING-PIVOT "ScalingPivot" :test #'string=)
(alexandria:define-constant FBX-ROTATION-PIVOT "RotationPivot" :test #'string=)
(alexandria:define-constant FBX-SCALING-OFFSET "ScalingOffset" :test #'string=)
(alexandria:define-constant FBX-ROTATION-OFFSET "RotationOffset" :test #'string=)
(alexandria:define-constant FBX-PRE-ROTATION "PreRotation" :test #'string=)
(alexandria:define-constant FBX-POST-ROTATION "PostRotation" :test #'string=)
(alexandria:define-constant FBX-VISIBILITY "Visibility" :test #'string=)
(alexandria:define-constant FBX-WEIGHT "Weight" :test #'string=)

(cffi:defcenum rotation-order
  :xyz
  :xzy
  :yzx
  :yxz
  :zxy
  :zyx
  :spheric)

(cffi:defcenum dom-value-type
  :number
  :string
  :array-i8
  :array-i32
  :array-i64
  :array-f32
  :array-f64
  :array-raw-string
  :array-ignored)

(cffi:defcenum prop-type
  :unknown
  :boolean
  :integer
  :number
  :vector
  :color
  :color-with-alpha
  :string
  :date-time
  :translation
  :rotation
  :scaling
  :distance
  :compound
  :blob
  :reference)

(cffi:defcenum prop-flag
  (:animatable #x1)
  (:user-defined #x2)
  (:hidden #x4)
  (:lock-x #x10)
  (:lock-y #x20)
  (:lock-z #x40)
  (:lock-w #x80)
  (:mute-x #x100)
  (:mute-y #x200)
  (:mute-z #x400)
  (:mute-w #x800)
  (:synthetic #x1000)
  (:animated #x2000)
  (:not-found #x4000)
  (:connected #x8000)
  (:no-value #x10000)
  (:overridden #x20000)
  (:value-real #x100000)
  (:value-vec2 #x200000)
  (:value-vec3 #x400000)
  (:value-vec4 #x800000)
  (:value-int #x1000000)
  (:value-str #x2000000)
  (:value-blob #x4000000))

(cffi:defcenum element-type
  :unknown
  :node
  :mesh
  :light
  :camera
  :bone
  :empty
  :line-curve
  :nurbs-curve
  :nurbs-surface
  :nurbs-trim-surface
  :nurbs-trim-boundary
  :procedural-geometry
  :stereo-camera
  :camera-switcher
  :marker
  :lod-group
  :skin-deformer
  :skin-cluster
  :blend-deformer
  :blend-channel
  :blend-shape
  :cache-deformer
  :cache-file
  :material
  :texture
  :video
  :shader
  :shader-binding
  :anim-stack
  :anim-layer
  :anim-value
  :anim-curve
  :display-layer
  :selection-set
  :selection-node
  :character
  :constraint
  :pose
  :metadata-object
  (:first-attrib 2 #|:mesh|#) ;; This sucks. FIXME upstream.
  (:last-attrib 16 #|:lod-group|#))

(cffi:defcenum inherit-type
  :no-shear
  :normal
  :no-scale)

(cffi:defcenum subdivision-display-mode
  :disabled
  :hull
  :hull-and-smooth
  :smooth)

(cffi:defcenum subdivision-boundary
  :default
  :legacy
  :sharp-corners
  :sharp-none
  :sharp-boundary
  :sharp-interior)

(cffi:defcenum light-type
  :point
  :directional
  :spot
  :area
  :volume)

(cffi:defcenum light-decay
  :none
  :linear
  :quadratic
  :cubic)

(cffi:defcenum light-area-shape
  :rectangle
  :sphere)

(cffi:defcenum projection-mode
  :perspective
  :orthographic)

(cffi:defcenum aspect-mode
  :window-size
  :fixed-ratio
  :fixed-resolution
  :fixed-width
  :fixed-height)

(cffi:defcenum aperture-mode
  :horizontal-and-vertical
  :horizontal
  :vertical
  :focal-length)

(cffi:defcenum gate-fit
  :none
  :vertical
  :horizontal
  :fill
  :overscan
  :stretch)

(cffi:defcenum aperture-format
  :custom
  :16mm-theatrical
  :super-16mm
  :35mm-academy
  :35mm-tv-projection
  :35mm-full-aperture
  :35mm-185-projection
  :35mm-anamorphic
  :70mm-projection
  :vistavision
  :dynavision
  :imax)

(cffi:defcenum coordinate-axis
  :positive-x
  :negative-x
  :positive-y
  :negative-y
  :positive-z
  :negative-z
  :unknown)

(cffi:defcenum nurbs-topology
  :open
  :periodic
  :closed)

(cffi:defcenum marker-type
  :unknown
  :fk-effector
  :ik-effector)

(cffi:defcenum lod-display
  :use-lod
  :show
  :hide)

(cffi:defcenum skinning-method
  :linear
  :rigid
  :dual-quaternion
  :blended-dq-linear)

(cffi:defcenum cache-file-format
  :unknown
  :pc2
  :mc)

(cffi:defcenum cache-data-format
  :unknown
  :real-float
  :vec3-float
  :real-double
  :vec3-double)

(cffi:defcenum cache-data-encoding
  :unknown
  :little-endian
  :big-endian)

(cffi:defcenum cache-interpretation
  :unknown
  :points
  :vertex-position
  :vertex-normal)

(cffi:defcenum shader-type
  :unknown
  :fbx-lambert
  :fbx-phong
  :osl-standard-surface
  :arnold-standard-surface
  :3ds-max-physical-material
  :3ds-max-pbr-metal-rough
  :3ds-max-pbr-spec-gloss
  :gltf-material
  :shaderfx-graph
  :blender-phong
  :wavefront-mtl)

(cffi:defcenum material-fbx-map
  :diffuse-factor
  :diffuse-color
  :specular-factor
  :specular-color
  :specular-exponent
  :reflection-factor
  :reflection-color
  :transparency-factor
  :transparency-color
  :emission-factor
  :emission-color
  :ambient-factor
  :ambient-color
  :normal-map
  :bump
  :bump-factor
  :displacement-factor
  :displacement
  :vector-displacement-factor
  :vector-displacement)

(cffi:defcenum material-pbr-map
  :base-factor
  :base-color
  :roughness
  :metalness
  :diffuse-roughness
  :specular-factor
  :specular-color
  :specular-ior
  :specular-anisotropy
  :specular-rotation
  :transmission-factor
  :transmission-color
  :transmission-depth
  :transmission-scatter
  :transmission-scatter-anisotropy
  :transmission-dispersion
  :transmission-roughness
  :transmission-extra-roughness
  :transmission-priority
  :transmission-enable-in-aov
  :subsurface-factor
  :subsurface-color
  :subsurface-radius
  :subsurface-scale
  :subsurface-anisotropy
  :subsurface-tint-color
  :subsurface-type
  :sheen-factor
  :sheen-color
  :sheen-roughness
  :coat-factor
  :coat-color
  :coat-roughness
  :coat-ior
  :coat-anisotropy
  :coat-rotation
  :coat-normal
  :coat-affect-base-color
  :coat-affect-base-roughness
  :thin-film-thickness
  :thin-film-ior
  :emission-factor
  :emission-color
  :opacity
  :indirect-diffuse
  :indirect-specular
  :normal-map
  :tangent-map
  :displacement-map
  :matte-factor
  :matte-color
  :ambient-occlusion
  :glossiness
  :coat-glossiness
  :transmission-glossiness)

(cffi:defcenum material-feature
  :pbr
  :metalness
  :diffuse
  :specular
  :emission
  :transmission
  :coat
  :sheen
  :opacity
  :ambient-occlusion
  :matte
  :unlit
  :ior
  :diffuse-roughness
  :transmission-roughness
  :thin-walled
  :caustics
  :exit-to-background
  :internal-reflections
  :double-sided
  :roughness-as-glossiness
  :coat-roughness-as-glossiness
  :transmission-roughness-as-glossiness)

(cffi:defcenum texture-type
  :file
  :layered
  :procedural
  :shader)

(cffi:defcenum blend-mode
  :translucent
  :additive
  :multiply
  :multiply-2x
  :over
  :replace
  :dissolve
  :darken
  :color-burn
  :linear-burn
  :darker-color
  :lighten
  :screen
  :color-dodge
  :linear-dodge
  :lighter-color
  :soft-light
  :hard-light
  :vivid-light
  :linear-light
  :pin-light
  :hard-mix
  :difference
  :exclusion
  :subtract
  :divide
  :hue
  :saturation
  :color
  :luminosity
  :overlay)

(cffi:defcenum wrap-mode
  :repeat
  :clamp)

(cffi:defcenum shader-texture-type
  :unknown
  :select-output
  :osl)

(cffi:defcenum interpolation
  :constant-prev
  :constant-next
  :linear
  :cubic)

(cffi:defcenum constraint-type
  :unknown
  :aim
  :parent
  :position
  :rotation
  :scale
  :single-chain-ik)

(cffi:defcenum aim-up-type
  :scene
  :to-node
  :align-node
  :vector
  :none)

(cffi:defcenum ik-pole-type
  :vector
  :node)

(cffi:defcenum exporter
  :unknown
  :fbx-sdk
  :blender-binary
  :blender-ascii
  :motion-builder
  :bc-unity-exporter)

(cffi:defcenum file-format
  :unknown
  :fbx
  :obj
  :mtl)

(cffi:defcenum warning-type
  :missing-external-file
  :implicit-mtl
  :truncated-array
  :index-clamped
  :bad-unicode
  :bad-element-connected-to-root
  :duplicate-object-id
  :empty-face-removed
  :unknown-obj-directive
  (:first-deduplicated 3 #|:index-clamped|#))

(cffi:defcenum time-mode
  :default
  :120-fps
  :100-fps
  :60-fps
  :50-fps
  :48-fps
  :30-fps
  :30-fps-drop
  :ntsc-drop-frame
  :ntsc-full-frame
  :pal
  :24-fps
  :1000-fps
  :film-full-frame
  :custom
  :96-fps
  :72-fps
  :59-94-fps)

(cffi:defcenum time-protocol
  :smpte
  :frame-count
  :default)

(cffi:defcenum snap-mode
  :none
  :snap
  :play
  :snap-and-play)

(cffi:defcenum topo-flags
  (:topo-non-manifold #x1))

(cffi:defcenum open-file-type
  :main-model
  :geometry-cache
  :obj-mtl)

(cffi:defcenum error-type
  :none
  :unknown
  :file-not-found
  :out-of-memory
  :memory-limit
  :allocation-limit
  :truncated-file
  :io
  :cancelled
  :unrecognized-file-format
  :uninitialized-options
  :zero-vertex-size
  :invalid-utf8
  :feature-disabled
  :bad-nurbs
  :bad-index
  :unsafe-options)

(cffi:defcenum progress-result
  (:continue #x100)
  (:cancel #x200))

(cffi:defcenum index-error-handling
  :clamp
  :no-index
  :abort-loading
  :unsafe-ignore)

(cffi:defcenum unicode-error-handling
  :replacement-character
  :underscore
  :question-mark
  :remove
  :abort-loading
  :unsafe-ignore)

(cffi:defcenum geometry-transform-handling
  :preserve
  :helper-nodes
  :modify-geometry
  :modify-geometry-no-fallback)

(cffi:defcenum space-conversion
  :transform-root
  :adjust-transforms)

(cffi:defcstruct (string :conc-name string- :class string)
  (data :string)
  (length :size))

(cffi:defcstruct (blob :conc-name blob- :class blob)
  (data :pointer)
  (size :size))

(cffi:defcstruct (vec2 :conc-name vec2- :class vec2)
  (x :float)
  (y :float))

(cffi:defcstruct (vec3 :conc-name vec3- :class vec3)
  (x :float)
  (y :float)
  (z :float))

(cffi:defcstruct (vec4 :conc-name vec4- :class vec4)
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(cffi:defcstruct (quat :conc-name quat- :class quat)
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(cffi:defcstruct (transform :conc-name transform- :class transform)
  (translation (:struct vec3))
  (rotation (:struct quat))
  (scale (:struct vec3)))

(cffi:defcstruct (matrix :conc-name matrix- :class matrix)
  (v (:array :float 12)))

(cffi:defcstruct (dom-value :conc-name dom-value- :class dom-value)
  (type dom-value-type)
  (value-str (:struct string))
  (value-blob (:struct blob))
  (value-int :int64)
  (value-float :double))

(cffi:defcstruct (list :conc-name list- :class list)
  (data :pointer)
  (count :size))

(cffi:defcstruct (dom-node :conc-name dom-node- :class dom-node)
  (name (:struct string))
  (children (:struct list))
  (values (:struct list)))

(cffi:defcstruct (prop :conc-name prop- :class prop)
  (name (:struct string))
  (-internal-key :uint32)
  (type prop-type)
  (flags prop-flag)
  (value-str (:struct string))
  (value-blob (:struct blob))
  (value-int :int64)
  (value-vec4 (:struct vec4)))

(cffi:defcstruct (props :conc-name props- :class props)
  (props (:struct list))
  (num-animated :size)
  (defaults :pointer))

(cffi:defcstruct (connection :conc-name connection- :class connection)
  (src :pointer)
  (dst :pointer)
  (src-prop (:struct string))
  (dst-prop (:struct string)))

(cffi:defcstruct (element :conc-name element- :class element)
  (name (:struct string))
  (props (:struct props))
  (element-id :uint32)
  (typed-id :uint32)
  (instances (:struct list))
  (type element-type)
  (connections-src (:struct list))
  (connections-dst (:struct list))
  (dom-node :pointer)
  (scene :pointer))

(cffi:defcstruct (unknown :conc-name unknown- :class unknown)
  (element (:struct element))
  (type (:struct string))
  (super-type (:struct string))
  (sub-type (:struct string)))

(cffi:defcstruct (node :conc-name node- :class node)
  (element (:struct element))
  (parent :pointer)
  (children (:struct list))
  (mesh :pointer)
  (light :pointer)
  (camera :pointer)
  (bone :pointer)
  (attrib :pointer)
  (geometry-transform-helper :pointer)
  (attrib-type element-type)
  (all-attribs (:struct list))
  (inherit-type inherit-type)
  (local-transform (:struct transform))
  (geometry-transform (:struct transform))
  (rotation-order rotation-order)
  (euler-rotation (:struct vec3))
  (world-transform (:struct transform))
  (node-to-parent (:struct matrix))
  (node-to-world (:struct matrix))
  (geometry-to-node (:struct matrix))
  (geometry-to-world (:struct matrix))
  (adjust-pre-rotation (:struct quat))
  (adjust-pre-scale (:struct vec3))
  (adjust-post-rotation (:struct quat))
  (materials (:struct list))
  (visible :bool)
  (is-root :bool)
  (has-geometry-transform :bool)
  (has-adjust-transform :bool)
  (is-geometry-transform-helper :bool)
  (node-depth :uint32))

(cffi:defcstruct (vertex-attrib :conc-name vertex-attrib- :class vertex-attrib)
  (exists :bool)
  (values (:struct list))
  (indices (:struct list))
  (value-reals :size)
  (unique-per-vertex :bool))

(cffi:defcstruct (vertex-real :conc-name vertex-real- :class vertex-real)
  (exists :bool)
  (values (:struct list))
  (indices (:struct list))
  (value-reals :size)
  (unique-per-vertex :bool))

(cffi:defcstruct (vertex-vec2 :conc-name vertex-vec2- :class vertex-vec2)
  (exists :bool)
  (values (:struct list))
  (indices (:struct list))
  (value-reals :size)
  (unique-per-vertex :bool))

(cffi:defcstruct (vertex-vec3 :conc-name vertex-vec3- :class vertex-vec3)
  (exists :bool)
  (values (:struct list))
  (indices (:struct list))
  (value-reals :size)
  (unique-per-vertex :bool))

(cffi:defcstruct (vertex-vec4 :conc-name vertex-vec4- :class vertex-vec4)
  (exists :bool)
  (values (:struct list))
  (indices (:struct list))
  (value-reals :size)
  (unique-per-vertex :bool))

(cffi:defcstruct (uv-set :conc-name uv-set- :class uv-set)
  (name (:struct string))
  (index :uint32)
  (vertex-uv (:struct vertex-vec2))
  (vertex-tangent (:struct vertex-vec3))
  (vertex-bitangent (:struct vertex-vec3)))

(cffi:defcstruct (color-set :conc-name color-set- :class color-set)
  (name (:struct string))
  (index :uint32)
  (vertex-color (:struct vertex-vec4)))

(cffi:defcstruct (edge :conc-name edge- :class edge)
  (a :uint32)
  (b :uint32))

(cffi:defcstruct (face :conc-name face- :class face)
  (index-begin :uint32)
  (num-indices :uint32))

(cffi:defcstruct (mesh-material :conc-name mesh-material- :class mesh-material)
  (material :pointer)
  (num-faces :size)
  (num-triangles :size)
  (num-empty-faces :size)
  (num-point-faces :size)
  (num-line-faces :size)
  (face-indices (:struct list)))

(cffi:defcstruct (face-group :conc-name face-group- :class face-group)
  (id :int32)
  (name (:struct string))
  (num-faces :size)
  (num-triangles :size)
  (face-indices (:struct list)))

(cffi:defcstruct (subdivision-weight-range :conc-name subdivision-weight-range- :class subdivision-weight-range)
  (weight-begin :uint32)
  (num-weights :uint32))

(cffi:defcstruct (subdivision-weight :conc-name subdivision-weight- :class subdivision-weight)
  (weight :float)
  (index :uint32))

(cffi:defcstruct (subdivision-result :conc-name subdivision-result- :class subdivision-result)
  (result-memory-used :size)
  (temp-memory-used :size)
  (result-allocs :size)
  (temp-allocs :size)
  (source-vertex-ranges (:struct list))
  (source-vertex-weights (:struct list))
  (skin-cluster-ranges (:struct list))
  (skin-cluster-weights (:struct list)))

(cffi:defcstruct (mesh :conc-name mesh- :class mesh)
  (element (:struct element))
  (num-vertices :size)
  (num-indices :size)
  (num-faces :size)
  (num-triangles :size)
  (num-edges :size)
  (max-face-triangles :size)
  (num-empty-faces :size)
  (num-point-faces :size)
  (num-line-faces :size)
  (faces (:struct list))
  (face-smoothing (:struct list))
  (face-material (:struct list))
  (face-group (:struct list))
  (face-hole (:struct list))
  (edges (:struct list))
  (edge-smoothing (:struct list))
  (edge-crease (:struct list))
  (edge-visibility (:struct list))
  (vertex-indices (:struct list))
  (vertices (:struct list))
  (vertex-first-index (:struct list))
  (vertex-position (:struct vertex-vec3))
  (vertex-normal (:struct vertex-vec3))
  (vertex-uv (:struct vertex-vec2))
  (vertex-tangent (:struct vertex-vec3))
  (vertex-bitangent (:struct vertex-vec3))
  (vertex-color (:struct vertex-vec4))
  (vertex-crease (:struct vertex-real))
  (uv-sets (:struct list))
  (color-sets (:struct list))
  (materials (:struct list))
  (face-groups (:struct list))
  (skinned-is-local :bool)
  (skinned-position (:struct vertex-vec3))
  (skinned-normal (:struct vertex-vec3))
  (skin-deformers (:struct list))
  (blend-deformers (:struct list))
  (cache-deformers (:struct list))
  (all-deformers (:struct list))
  (subdivision-preview-levels :uint32)
  (subdivision-render-levels :uint32)
  (subdivision-display-mode subdivision-display-mode)
  (subdivision-boundary subdivision-boundary)
  (subdivision-uv-boundary subdivision-boundary)
  (generated-normals :bool)
  (subdivision-evaluated :bool)
  (subdivision-result :pointer)
  (from-tessellated-nurbs :bool))

(cffi:defcstruct (light :conc-name light- :class light)
  (element (:struct element))
  (color (:struct vec3))
  (intensity :float)
  (local-direction (:struct vec3))
  (type light-type)
  (decay light-decay)
  (area-shape light-area-shape)
  (inner-angle :float)
  (outer-angle :float)
  (cast-light :bool)
  (cast-shadows :bool))

(cffi:defcstruct (coordinate-axes :conc-name coordinate-axes- :class coordinate-axes)
  (right coordinate-axis)
  (up coordinate-axis)
  (front coordinate-axis))

(cffi:defcstruct (camera :conc-name camera- :class camera)
  (element (:struct element))
  (projection-mode projection-mode)
  (resolution-is-pixels :bool)
  (resolution (:struct vec2))
  (field-of-view-deg (:struct vec2))
  (field-of-view-tan (:struct vec2))
  (orthographic-extent :float)
  (orthographic-size (:struct vec2))
  (projection-plane (:struct vec2))
  (aspect-ratio :float)
  (near-plane :float)
  (far-plane :float)
  (projection-axes (:struct coordinate-axes))
  (aspect-mode aspect-mode)
  (aperture-mode aperture-mode)
  (gate-fit gate-fit)
  (aperture-format aperture-format)
  (focal-length-mm :float)
  (film-size-inch (:struct vec2))
  (aperture-size-inch (:struct vec2))
  (squeeze-ratio :float))

(cffi:defcstruct (bone :conc-name bone- :class bone)
  (element (:struct element))
  (radius :float)
  (relative-length :float)
  (is-root :bool))

(cffi:defcstruct (empty :conc-name empty- :class empty)
  (element (:struct element)))

(cffi:defcstruct (line-segment :conc-name line-segment- :class line-segment)
  (index-begin :uint32)
  (num-indices :uint32))

(cffi:defcstruct (line-curve :conc-name line-curve- :class line-curve)
  (element (:struct element))
  (color (:struct vec3))
  (control-points (:struct list))
  (point-indices (:struct list))
  (segments (:struct list))
  (from-tessellated-nurbs :bool))

(cffi:defcstruct (nurbs-basis :conc-name nurbs-basis- :class nurbs-basis)
  (order :uint32)
  (topology nurbs-topology)
  (knot-vector (:struct list))
  (t-min :float)
  (t-max :float)
  (spans (:struct list))
  (is-2d :bool)
  (num-wrap-control-points :size)
  (valid :bool))

(cffi:defcstruct (nurbs-curve :conc-name nurbs-curve- :class nurbs-curve)
  (element (:struct element))
  (basis (:struct nurbs-basis))
  (control-points (:struct list)))

(cffi:defcstruct (nurbs-surface :conc-name nurbs-surface- :class nurbs-surface)
  (element (:struct element))
  (basis-u (:struct nurbs-basis))
  (basis-v (:struct nurbs-basis))
  (num-control-points-u :size)
  (num-control-points-v :size)
  (control-points (:struct list))
  (span-subdivision-u :uint32)
  (span-subdivision-v :uint32)
  (flip-normals :bool)
  (material :pointer))

(cffi:defcstruct (nurbs-trim-surface :conc-name nurbs-trim-surface- :class nurbs-trim-surface)
  (element (:struct element)))

(cffi:defcstruct (nurbs-trim-boundary :conc-name nurbs-trim-boundary- :class nurbs-trim-boundary)
  (element (:struct element)))

(cffi:defcstruct (procedural-geometry :conc-name procedural-geometry- :class procedural-geometry)
  (element (:struct element)))

(cffi:defcstruct (stereo-camera :conc-name stereo-camera- :class stereo-camera)
  (element (:struct element))
  (left :pointer)
  (right :pointer))

(cffi:defcstruct (camera-switcher :conc-name camera-switcher- :class camera-switcher)
  (element (:struct element)))

(cffi:defcstruct (marker :conc-name marker- :class marker)
  (element (:struct element))
  (type marker-type))

(cffi:defcstruct (lod-level :conc-name lod-level- :class lod-level)
  (distance :float)
  (display lod-display))

(cffi:defcstruct (lod-group :conc-name lod-group- :class lod-group)
  (element (:struct element))
  (relative-distances :bool)
  (lod-levels (:struct list))
  (ignore-parent-transform :bool)
  (use-distance-limit :bool)
  (distance-limit-min :float)
  (distance-limit-max :float))

(cffi:defcstruct (skin-vertex :conc-name skin-vertex- :class skin-vertex)
  (weight-begin :uint32)
  (num-weights :uint32)
  (dq-weight :float))

(cffi:defcstruct (skin-weight :conc-name skin-weight- :class skin-weight)
  (cluster-index :uint32)
  (weight :float))

(cffi:defcstruct (skin-deformer :conc-name skin-deformer- :class skin-deformer)
  (element (:struct element))
  (skinning-method skinning-method)
  (clusters (:struct list))
  (vertices (:struct list))
  (weights (:struct list))
  (max-weights-per-vertex :size)
  (num-dq-weights :size)
  (dq-vertices (:struct list))
  (dq-weights (:struct list)))

(cffi:defcstruct (skin-cluster :conc-name skin-cluster- :class skin-cluster)
  (element (:struct element))
  (bone-node :pointer)
  (geometry-to-bone (:struct matrix))
  (mesh-node-to-bone (:struct matrix))
  (bind-to-world (:struct matrix))
  (geometry-to-world (:struct matrix))
  (geometry-to-world-transform (:struct transform))
  (num-weights :size)
  (vertices (:struct list))
  (weights (:struct list)))

(cffi:defcstruct (blend-deformer :conc-name blend-deformer- :class blend-deformer)
  (element (:struct element))
  (channels (:struct list)))

(cffi:defcstruct (blend-keyframe :conc-name blend-keyframe- :class blend-keyframe)
  (shape :pointer)
  (target-weight :float)
  (effective-weight :float))

(cffi:defcstruct (blend-channel :conc-name blend-channel- :class blend-channel)
  (element (:struct element))
  (weight :float)
  (keyframes (:struct list)))

(cffi:defcstruct (blend-shape :conc-name blend-shape- :class blend-shape)
  (element (:struct element))
  (num-offsets :size)
  (offset-vertices (:struct list))
  (position-offsets (:struct list))
  (normal-offsets (:struct list)))

(cffi:defcstruct (cache-frame :conc-name cache-frame- :class cache-frame)
  (channel (:struct string))
  (time :double)
  (filename (:struct string))
  (file-format cache-file-format)
  (data-format cache-data-format)
  (data-encoding cache-data-encoding)
  (data-offset :uint64)
  (data-count :uint32)
  (data-element-bytes :uint32)
  (data-total-bytes :uint64))

(cffi:defcstruct (cache-channel :conc-name cache-channel- :class cache-channel)
  (name (:struct string))
  (interpretation cache-interpretation)
  (interpretation-name (:struct string))
  (frames (:struct list)))

(cffi:defcstruct (geometry-cache :conc-name geometry-cache- :class geometry-cache)
  (root-filename (:struct string))
  (channels (:struct list))
  (frames (:struct list))
  (extra-info (:struct list)))

(cffi:defcstruct (cache-deformer :conc-name cache-deformer- :class cache-deformer)
  (element (:struct element))
  (channel (:struct string))
  (file :pointer)
  (external-cache :pointer)
  (external-channel :pointer))

(cffi:defcstruct (cache-file :conc-name cache-file- :class cache-file)
  (element (:struct element))
  (filename (:struct string))
  (absolute-filename (:struct string))
  (relative-filename (:struct string))
  (raw-filename (:struct blob))
  (raw-absolute-filename (:struct blob))
  (raw-relative-filename (:struct blob))
  (format cache-file-format)
  (external-cache :pointer))

(cffi:defcstruct (material-map :conc-name material-map- :class material-map)
  (value-vec4 (:struct vec4))
  (value-int :int64)
  (texture :pointer)
  (has-value :bool)
  (texture-enabled :bool)
  (feature-disabled :bool)
  (value-components :uint8))

(cffi:defcstruct (material-feature-info :conc-name material-feature-info- :class material-feature-info)
  (enabled :bool)
  (is-explicit :bool))

(cffi:defcstruct (material-texture :conc-name material-texture- :class material-texture)
  (material-prop (:struct string))
  (shader-prop (:struct string))
  (texture :pointer))

(cffi:defcstruct (material-fbx-maps :conc-name material-fbx-maps- :class material-fbx-maps)
  (diffuse-factor (:struct material-map))
  (diffuse-color (:struct material-map))
  (specular-factor (:struct material-map))
  (specular-color (:struct material-map))
  (specular-exponent (:struct material-map))
  (reflection-factor (:struct material-map))
  (reflection-color (:struct material-map))
  (transparency-factor (:struct material-map))
  (transparency-color (:struct material-map))
  (emission-factor (:struct material-map))
  (emission-color (:struct material-map))
  (ambient-factor (:struct material-map))
  (ambient-color (:struct material-map))
  (normal-map (:struct material-map))
  (bump (:struct material-map))
  (bump-factor (:struct material-map))
  (displacement-factor (:struct material-map))
  (displacement (:struct material-map))
  (vector-displacement-factor (:struct material-map))
  (vector-displacement (:struct material-map)))

(cffi:defcstruct (material-pbr-maps :conc-name material-pbr-maps- :class material-pbr-maps)
  (base-factor (:struct material-map))
  (base-color (:struct material-map))
  (roughness (:struct material-map))
  (metalness (:struct material-map))
  (diffuse-roughness (:struct material-map))
  (specular-factor (:struct material-map))
  (specular-color (:struct material-map))
  (specular-ior (:struct material-map))
  (specular-anisotropy (:struct material-map))
  (specular-rotation (:struct material-map))
  (transmission-factor (:struct material-map))
  (transmission-color (:struct material-map))
  (transmission-depth (:struct material-map))
  (transmission-scatter (:struct material-map))
  (transmission-scatter-anisotropy (:struct material-map))
  (transmission-dispersion (:struct material-map))
  (transmission-roughness (:struct material-map))
  (transmission-extra-roughness (:struct material-map))
  (transmission-priority (:struct material-map))
  (transmission-enable-in-aov (:struct material-map))
  (subsurface-factor (:struct material-map))
  (subsurface-color (:struct material-map))
  (subsurface-radius (:struct material-map))
  (subsurface-scale (:struct material-map))
  (subsurface-anisotropy (:struct material-map))
  (subsurface-tint-color (:struct material-map))
  (subsurface-type (:struct material-map))
  (sheen-factor (:struct material-map))
  (sheen-color (:struct material-map))
  (sheen-roughness (:struct material-map))
  (coat-factor (:struct material-map))
  (coat-color (:struct material-map))
  (coat-roughness (:struct material-map))
  (coat-ior (:struct material-map))
  (coat-anisotropy (:struct material-map))
  (coat-rotation (:struct material-map))
  (coat-normal (:struct material-map))
  (coat-affect-base-color (:struct material-map))
  (coat-affect-base-roughness (:struct material-map))
  (thin-film-thickness (:struct material-map))
  (thin-film-ior (:struct material-map))
  (emission-factor (:struct material-map))
  (emission-color (:struct material-map))
  (opacity (:struct material-map))
  (indirect-diffuse (:struct material-map))
  (indirect-specular (:struct material-map))
  (normal-map (:struct material-map))
  (tangent-map (:struct material-map))
  (displacement-map (:struct material-map))
  (matte-factor (:struct material-map))
  (matte-color (:struct material-map))
  (ambient-occlusion (:struct material-map))
  (glossiness (:struct material-map))
  (coat-glossiness (:struct material-map))
  (transmission-glossiness (:struct material-map)))

(cffi:defcstruct (material-features :conc-name material-features- :class material-features)
  (pbr (:struct material-feature-info))
  (metalness (:struct material-feature-info))
  (diffuse (:struct material-feature-info))
  (specular (:struct material-feature-info))
  (emission (:struct material-feature-info))
  (transmission (:struct material-feature-info))
  (coat (:struct material-feature-info))
  (sheen (:struct material-feature-info))
  (opacity (:struct material-feature-info))
  (ambient-occlusion (:struct material-feature-info))
  (matte (:struct material-feature-info))
  (unlit (:struct material-feature-info))
  (ior (:struct material-feature-info))
  (diffuse-roughness (:struct material-feature-info))
  (transmission-roughness (:struct material-feature-info))
  (thin-walled (:struct material-feature-info))
  (caustics (:struct material-feature-info))
  (exit-to-background (:struct material-feature-info))
  (internal-reflections (:struct material-feature-info))
  (double-sided (:struct material-feature-info))
  (roughness-as-glossiness (:struct material-feature-info))
  (coat-roughness-as-glossiness (:struct material-feature-info))
  (transmission-roughness-as-glossiness (:struct material-feature-info)))

(cffi:defcstruct (material :conc-name material- :class material)
  (element (:struct element))
  (fbx (:struct material-fbx-maps))
  (pbr (:struct material-pbr-maps))
  (features (:struct material-features))
  (shader-type shader-type)
  (shader :pointer)
  (shading-model-name (:struct string))
  (shader-prop-prefix (:struct string))
  (textures (:struct list)))

(cffi:defcstruct (texture-layer :conc-name texture-layer- :class texture-layer)
  (texture :pointer)
  (blend-mode blend-mode)
  (alpha :float))

(cffi:defcstruct (shader-texture-input :conc-name shader-texture-input- :class shader-texture-input)
  (name (:struct string))
  (value-vec4 (:struct vec4))
  (value-int :int64)
  (value-str (:struct string))
  (value-blob (:struct blob))
  (texture :pointer)
  (texture-output-index :int64)
  (texture-enabled :bool)
  (prop :pointer)
  (texture-prop :pointer)
  (texture-enabled-prop :pointer))

(cffi:defcstruct (shader-texture :conc-name shader-texture- :class shader-texture)
  (type shader-texture-type)
  (shader-name (:struct string))
  (shader-type-id :uint64)
  (inputs (:struct list))
  (shader-source (:struct string))
  (raw-shader-source (:struct blob))
  (main-texture :pointer)
  (main-texture-output-index :int64)
  (prop-prefix (:struct string)))

(cffi:defcstruct (texture-file :conc-name texture-file- :class texture-file)
  (idx :uint32)
  (filename (:struct string))
  (absolute-filename (:struct string))
  (relative-filename (:struct string))
  (raw-filename (:struct blob))
  (raw-absolute-filename (:struct blob))
  (raw-relative-filename (:struct blob))
  (content (:struct blob)))

(cffi:defcstruct (texture :conc-name texture- :class texture)
  (element (:struct element))
  (type texture-type)
  (filename (:struct string))
  (absolute-filename (:struct string))
  (relative-filename (:struct string))
  (raw-filename (:struct blob))
  (raw-absolute-filename (:struct blob))
  (raw-relative-filename (:struct blob))
  (content (:struct blob))
  (video :pointer)
  (file-index :uint32)
  (has-file :bool)
  (layers (:struct list))
  (shader :pointer)
  (file-textures (:struct list))
  (uv-set (:struct string))
  (wrap-u wrap-mode)
  (wrap-v wrap-mode)
  (has-uv-transform :bool)
  (uv-transform (:struct transform))
  (texture-to-uv (:struct matrix))
  (uv-to-texture (:struct matrix)))

(cffi:defcstruct (video :conc-name video- :class video)
  (element (:struct element))
  (filename (:struct string))
  (absolute-filename (:struct string))
  (relative-filename (:struct string))
  (raw-filename (:struct blob))
  (raw-absolute-filename (:struct blob))
  (raw-relative-filename (:struct blob))
  (content (:struct blob)))

(cffi:defcstruct (shader :conc-name shader- :class shader)
  (element (:struct element))
  (type shader-type)
  (bindings (:struct list)))

(cffi:defcstruct (shader-prop-binding :conc-name shader-prop-binding- :class shader-prop-binding)
  (shader-prop (:struct string))
  (material-prop (:struct string)))

(cffi:defcstruct (shader-binding :conc-name shader-binding- :class shader-binding)
  (element (:struct element))
  (prop-bindings (:struct list)))

(cffi:defcstruct (anim-layer-desc :conc-name anim-layer-desc- :class anim-layer-desc)
  (layer :pointer)
  (weight :float))

(cffi:defcstruct (prop-override :conc-name prop-override- :class prop-override)
  (element-id :uint32)
  (prop-name :string)
  (value (:struct vec3))
  (value-str :string)
  (value-int :int64)
  (-internal-key :uint32))

(cffi:defcstruct (anim :conc-name anim- :class anim)
  (layers (:struct list))
  (prop-overrides (:struct list))
  (ignore-connections :bool)
  (time-begin :double)
  (time-end :double))

(cffi:defcstruct (anim-stack :conc-name anim-stack- :class anim-stack)
  (element (:struct element))
  (time-begin :double)
  (time-end :double)
  (layers (:struct list))
  (anim (:struct anim)))

(cffi:defcstruct (anim-prop :conc-name anim-prop- :class anim-prop)
  (element :pointer)
  (-internal-key :uint32)
  (prop-name (:struct string))
  (anim-value :pointer))

(cffi:defcstruct (anim-layer :conc-name anim-layer- :class anim-layer)
  (element (:struct element))
  (weight :float)
  (weight-is-animated :bool)
  (blended :bool)
  (additive :bool)
  (compose-rotation :bool)
  (compose-scale :bool)
  (anim-values (:struct list))
  (anim-props (:struct list))
  (anim (:struct anim))
  (-min-element-id :uint32)
  (-max-element-id :uint32)
  (-element-id-bitmask :uint32 :count 4))

(cffi:defcstruct (anim-value :conc-name anim-value- :class anim-value)
  (element (:struct element))
  (default-value (:struct vec3))
  (curves :pointer :count 3))

(cffi:defcstruct (tangent :conc-name tangent- :class tangent)
  (dx :float)
  (dy :float))

(cffi:defcstruct (keyframe :conc-name keyframe- :class keyframe)
  (time :double)
  (value :float)
  (interpolation interpolation)
  (left (:struct tangent))
  (right (:struct tangent)))

(cffi:defcstruct (anim-curve :conc-name anim-curve- :class anim-curve)
  (element (:struct element))
  (keyframes (:struct list)))

(cffi:defcstruct (display-layer :conc-name display-layer- :class display-layer)
  (element (:struct element))
  (nodes (:struct list))
  (visible :bool)
  (frozen :bool)
  (ui-color (:struct vec3)))

(cffi:defcstruct (selection-set :conc-name selection-set- :class selection-set)
  (element (:struct element))
  (nodes (:struct list)))

(cffi:defcstruct (selection-node :conc-name selection-node- :class selection-node)
  (element (:struct element))
  (target-node :pointer)
  (target-mesh :pointer)
  (include-node :bool)
  (vertices (:struct list))
  (edges (:struct list))
  (faces (:struct list)))

(cffi:defcstruct (character :conc-name character- :class character)
  (element (:struct element)))

(cffi:defcstruct (constraint-target :conc-name constraint-target- :class constraint-target)
  (node :pointer)
  (weight :float)
  (transform (:struct transform)))

(cffi:defcstruct (constraint :conc-name constraint- :class constraint)
  (element (:struct element))
  (type constraint-type)
  (type-name (:struct string))
  (node :pointer)
  (targets (:struct list))
  (weight :float)
  (active :bool)
  (constrain-translation (:array :bool 3))
  (constrain-rotation (:array :bool 3))
  (constrain-scale (:array :bool 3))
  (transform-offset (:struct transform))
  (aim-vector (:struct vec3))
  (aim-up-type aim-up-type)
  (aim-up-node :pointer)
  (aim-up-vector (:struct vec3))
  (ik-effector :pointer)
  (ik-end-node :pointer)
  (ik-pole-vector (:struct vec3)))

(cffi:defcstruct (bone-pose :conc-name bone-pose- :class bone-pose)
  (bone-node :pointer)
  (bone-to-world (:struct matrix)))

(cffi:defcstruct (pose :conc-name pose- :class pose)
  (element (:struct element))
  (bind-pose :bool)
  (bone-poses (:struct list)))

(cffi:defcstruct (metadata-object :conc-name metadata-object- :class metadata-object)
  (element (:struct element)))

(cffi:defcstruct (name-element :conc-name name-element- :class name-element)
  (name (:struct string))
  (type element-type)
  (-internal-key :uint32)
  (element :pointer))

(cffi:defcstruct (application :conc-name application- :class application)
  (vendor (:struct string))
  (name (:struct string))
  (version (:struct string)))

(cffi:defcstruct (warning :conc-name warning- :class warning)
  (type warning-type)
  (description (:struct string))
  (count :size))

(cffi:defcstruct (metadata :conc-name metadata- :class metadata)
  (warnings (:struct list))
  (ascii :bool)
  (version :uint32)
  (file-format file-format)
  (may-contain-no-index :bool)
  (may-contain-null-materials :bool)
  (may-contain-missing-vertex-position :bool)
  (may-contain-broken-elements :bool)
  (is-unsafe :bool)
  (has-warning (:array :bool 10))
  (creator (:struct string))
  (big-endian :bool)
  (filename (:struct string))
  (relative-root (:struct string))
  (raw-filename (:struct blob))
  (raw-relative-root (:struct blob))
  (exporter exporter)
  (exporter-version :uint32)
  (scene-props (:struct props))
  (original-application (:struct application))
  (latest-application (:struct application))
  (geometry-ignored :bool)
  (animation-ignored :bool)
  (embedded-ignored :bool)
  (max-face-triangles :size)
  (result-memory-used :size)
  (temp-memory-used :size)
  (result-allocs :size)
  (temp-allocs :size)
  (element-buffer-size :size)
  (num-shader-textures :size)
  (bone-prop-size-unit :float)
  (bone-prop-limb-length-relative :bool)
  (ktime-to-sec :double)
  (original-file-path (:struct string))
  (raw-original-file-path (:struct blob)))

(cffi:defcstruct (scene-settings :conc-name scene-settings- :class scene-settings)
  (props (:struct props))
  (axes (:struct coordinate-axes))
  (unit-meters :float)
  (frames-per-second :double)
  (ambient-color (:struct vec3))
  (default-camera (:struct string))
  (time-mode time-mode)
  (time-protocol time-protocol)
  (snap-mode snap-mode)
  (original-axis-up coordinate-axis)
  (original-unit-meters :float))

(cffi:defcstruct (scene :conc-name scene- :class scene)
  (metadata (:struct metadata))
  (settings (:struct scene-settings))
  (root-node :pointer)
  (anim (:struct anim))
  (combined-anim (:struct anim))
  (unknowns (:struct list))
  (nodes (:struct list))
  (meshes (:struct list))
  (lights (:struct list))
  (cameras (:struct list))
  (bones (:struct list))
  (empties (:struct list))
  (line-curves (:struct list))
  (nurbs-curves (:struct list))
  (nurbs-surfaces (:struct list))
  (nurbs-trim-surfaces (:struct list))
  (nurbs-trim-boundaries (:struct list))
  (procedural-geometries (:struct list))
  (stereo-cameras (:struct list))
  (camera-switchers (:struct list))
  (markers (:struct list))
  (lod-groups (:struct list))
  (skin-deformers (:struct list))
  (skin-clusters (:struct list))
  (blend-deformers (:struct list))
  (blend-channels (:struct list))
  (blend-shapes (:struct list))
  (cache-deformers (:struct list))
  (cache-files (:struct list))
  (materials (:struct list))
  (textures (:struct list))
  (videos (:struct list))
  (shaders (:struct list))
  (shader-bindings (:struct list))
  (anim-stacks (:struct list))
  (anim-layers (:struct list))
  (anim-values (:struct list))
  (anim-curves (:struct list))
  (display-layers (:struct list))
  (selection-sets (:struct list))
  (selection-nodes (:struct list))
  (characters (:struct list))
  (constraints (:struct list))
  (poses (:struct list))
  (metadata-objects (:struct list))
  (texture-files (:struct list))
  (elements (:struct list))
  (connections-src (:struct list))
  (connections-dst (:struct list))
  (elements-by-name (:struct list))
  (dom-root :pointer))

(cffi:defcstruct (curve-point :conc-name curve-point- :class curve-point)
  (valid :bool)
  (position (:struct vec3))
  (derivative (:struct vec3)))

(cffi:defcstruct (surface-point :conc-name surface-point- :class surface-point)
  (valid :bool)
  (position (:struct vec3))
  (derivative-u (:struct vec3))
  (derivative-v (:struct vec3)))

(cffi:defcstruct (topo-edge :conc-name topo-edge- :class topo-edge)
  (index :uint32)
  (next :uint32)
  (prev :uint32)
  (twin :uint32)
  (face :uint32)
  (edge :uint32)
  (flags topo-flags))

(cffi:defcstruct (vertex-stream :conc-name vertex-stream- :class vertex-stream)
  (data :pointer)
  (vertex-size :size))

(cffi:defcstruct (allocator :conc-name allocator- :class allocator)
  (alloc-fn :pointer)
  (realloc-fn :pointer)
  (free-fn :pointer)
  (free-allocator-fn :pointer)
  (user :pointer))

(cffi:defcstruct (allocator-opts :conc-name allocator-opts- :class allocator-opts)
  (allocator (:struct allocator))
  (memory-limit :size)
  (allocation-limit :size)
  (huge-threshold :size)
  (max-chunk-size :size))

(cffi:defcstruct (stream :conc-name stream- :class stream)
  (read-fn :pointer)
  (skip-fn :pointer)
  (close-fn :pointer)
  (user :pointer))

(cffi:defcstruct (open-file-info :conc-name open-file-info- :class open-file-info)
  (type open-file-type)
  (temp-allocator (:struct allocator))
  (original-filename (:struct blob)))

(cffi:defcstruct (open-file-cb :conc-name open-file-cb- :class open-file-cb)
  (fn :pointer)
  (user :pointer))

(cffi:defcstruct (close-memory-cb :conc-name close-memory-cb- :class close-memory-cb)
  (fn :pointer)
  (user :pointer))

(cffi:defcstruct (open-memory-opts :conc-name open-memory-opts- :class open-memory-opts)
  (-begin-zero :uint32)
  (allocator (:struct allocator-opts))
  (no-copy :bool)
  (close-cb (:struct close-memory-cb))
  (-end-zero :uint32))

(cffi:defcstruct (error-frame :conc-name error-frame- :class error-frame)
  (source-line :uint32)
  (function (:struct string))
  (description (:struct string)))

(cffi:defcstruct (error :conc-name error- :class error)
  (type error-type)
  (description (:struct string))
  (stack-size :uint32)
  (stack (:array (:struct error-frame) 8))
  (info-length :size)
  (info (:array :char 256)))

(cffi:defcstruct (progress :conc-name progress- :class progress)
  (bytes-read :uint64)
  (bytes-total :uint64))

(cffi:defcstruct (progress-cb :conc-name progress-cb- :class progress-cb)
  (fn :pointer)
  (user :pointer))

(cffi:defcstruct (inflate-input :conc-name inflate-input- :class inflate-input)
  (total-size :size)
  (data :pointer)
  (data-size :size)
  (buffer :pointer)
  (buffer-size :size)
  (read-fn :pointer)
  (read-user :pointer)
  (progress-cb (:struct progress-cb))
  (progress-interval-hint :uint64)
  (progress-size-before :uint64)
  (progress-size-after :uint64)
  (no-header :bool)
  (no-checksum :bool)
  (internal-fast-bits :size))

(cffi:defcstruct (inflate-retain :conc-name inflate-retain- :class inflate-retain)
  (initialized :bool)
  (data (:array :uint64 1024)))

(cffi:defcstruct (load-opts :conc-name load-opts- :class load-opts)
  (-begin-zero :uint32)
  (temp-allocator (:struct allocator-opts))
  (result-allocator (:struct allocator-opts))
  (ignore-geometry :bool)
  (ignore-animation :bool)
  (ignore-embedded :bool)
  (ignore-all-content :bool)
  (evaluate-skinning :bool)
  (evaluate-caches :bool)
  (load-external-files :bool)
  (ignore-missing-external-files :bool)
  (skip-skin-vertices :bool)
  (clean-skin-weights :bool)
  (disable-quirks :bool)
  (strict :bool)
  (allow-unsafe :bool)
  (index-error-handling index-error-handling)
  (connect-broken-elements :bool)
  (allow-nodes-out-of-root :bool)
  (allow-null-material :bool)
  (allow-missing-vertex-position :bool)
  (allow-empty-faces :bool)
  (generate-missing-normals :bool)
  (open-main-file-with-default :bool)
  (path-separator :char)
  (file-size-estimate :uint64)
  (read-buffer-size :size)
  (filename (:struct string))
  (raw-filename (:struct blob))
  (progress-cb (:struct progress-cb))
  (progress-interval-hint :uint64)
  (open-file-cb (:struct open-file-cb))
  (geometry-transform-handling geometry-transform-handling)
  (space-conversion space-conversion)
  (target-axes (:struct coordinate-axes))
  (target-unit-meters :float)
  (target-camera-axes (:struct coordinate-axes))
  (target-light-axes (:struct coordinate-axes))
  (geometry-transform-helper-name (:struct string))
  (no-prop-unit-scaling :bool)
  (no-anim-curve-unit-scaling :bool)
  (normalize-normals :bool)
  (normalize-tangents :bool)
  (use-root-transform :bool)
  (root-transform (:struct transform))
  (unicode-error-handling unicode-error-handling)
  (retain-dom :bool)
  (file-format file-format)
  (file-format-lookahead :size)
  (no-format-from-content :bool)
  (no-format-from-extension :bool)
  (obj-search-mtl-by-filename :bool)
  (obj-merge-objects :bool)
  (obj-merge-groups :bool)
  (obj-split-groups :bool)
  (obj-mtl-path (:struct string))
  (obj-mtl-data (:struct blob))
  (-end-zero :uint32))

(cffi:defcstruct (evaluate-opts :conc-name evaluate-opts- :class evaluate-opts)
  (-begin-zero :uint32)
  (temp-allocator (:struct allocator-opts))
  (result-allocator (:struct allocator-opts))
  (evaluate-skinning :bool)
  (evaluate-caches :bool)
  (load-external-files :bool)
  (open-file-cb (:struct open-file-cb))
  (-end-zero :uint32))

(cffi:defcstruct (tessellate-curve-opts :conc-name tessellate-curve-opts- :class tessellate-curve-opts)
  (-begin-zero :uint32)
  (temp-allocator (:struct allocator-opts))
  (result-allocator (:struct allocator-opts))
  (span-subdivision :uint32)
  (-end-zero :uint32))

(cffi:defcstruct (tessellate-surface-opts :conc-name tessellate-surface-opts- :class tessellate-surface-opts)
  (-begin-zero :uint32)
  (temp-allocator (:struct allocator-opts))
  (result-allocator (:struct allocator-opts))
  (span-subdivision-u :uint32)
  (span-subdivision-v :uint32)
  (-end-zero :uint32))

(cffi:defcstruct (subdivide-opts :conc-name subdivide-opts- :class subdivide-opts)
  (-begin-zero :uint32)
  (temp-allocator (:struct allocator-opts))
  (result-allocator (:struct allocator-opts))
  (boundary subdivision-boundary)
  (uv-boundary subdivision-boundary)
  (ignore-normals :bool)
  (interpolate-normals :bool)
  (interpolate-tangents :bool)
  (evaluate-source-vertices :bool)
  (max-source-vertices :size)
  (evaluate-skin-weights :bool)
  (max-skin-weights :size)
  (skin-deformer-index :size)
  (-end-zero :uint32))

(cffi:defcstruct (geometry-cache-opts :conc-name geometry-cache-opts- :class geometry-cache-opts)
  (-begin-zero :uint32)
  (temp-allocator (:struct allocator-opts))
  (result-allocator (:struct allocator-opts))
  (open-file-cb (:struct open-file-cb))
  (frames-per-second :double)
  (-end-zero :uint32))

(cffi:defcstruct (geometry-cache-data-opts :conc-name geometry-cache-data-opts- :class geometry-cache-data-opts)
  (-begin-zero :uint32)
  (open-file-cb (:struct open-file-cb))
  (additive :bool)
  (use-weight :bool)
  (weight :float)
  (-end-zero :uint32))

(cffi:defcstruct (panic :conc-name panic- :class panic)
  (did-panic :bool)
  (message-length :size)
  (message (:array :char 128)))

(cffi:defcvar empty-string (:struct string))
(cffi:defcvar empty-blob (:struct blob))
(cffi:defcvar identity-matrix (:struct matrix))
(cffi:defcvar identity-transform (:struct transform))
(cffi:defcvar zero-vec2 (:struct vec2))
(cffi:defcvar zero-vec3 (:struct vec3))
(cffi:defcvar zero-vec4 (:struct vec4))
(cffi:defcvar identity-quat (:struct quat))
(cffi:defcvar axes-right-handed-y-up (:struct coordinate-axes))
(cffi:defcvar axes-right-handed-z-up (:struct coordinate-axes))
(cffi:defcvar axes-left-handed-y-up (:struct coordinate-axes))
(cffi:defcvar axes-left-handed-z-up (:struct coordinate-axes))
(cffi:defcvar element-type-size (:array :size 40))
(cffi:defcvar source-version :uint32)

(cffi:defcfun (is-thread-safe "is_thread_safe") :bool)

(cffi:defcfun (load-memory "ufbx_load_memory") :pointer
  (data :pointer)
  (data-size :size)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (load-file "ufbx_load_file") :pointer
  (filename :string)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (load-stdio "ufbx_load_stdio") :pointer
  (file :pointer)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (load-stdio-prefix "ufbx_load_stdio_prefix") :pointer
  (file :pointer)
  (prefix :pointer)
  (prefix-size :size)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (load-stream "ufbx_load_stream") :pointer
  (stream :pointer)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (load-stream-prefix "ufbx_load_stream_prefix") :pointer
  (stream :pointer)
  (prefix :pointer)
  (prefix-size :size)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (free-scene "ufbx_free_scene") :void
  (scene :pointer))

(cffi:defcfun (retain-scene "ufbx_retain_scene") :void
  (scene :pointer))

(cffi:defcfun (format-error "ufbx_format_error") :size
  (dst :pointer)
  (dst-size :size)
  (error :pointer))

(cffi:defcfun (find-prop "ufbx_find_prop_len") :pointer
  (props :pointer)
  (name :pointer)
  (name-len :size))

(cffi:defcfun (find-real "ufbx_find_real_len") :float
  (props :pointer)
  (name :pointer)
  (name-len :size)
  (def :float))

(cffi:defcfun (find-int "ufbx_find_int_len") :int64
  (props :pointer)
  (name :pointer)
  (name_len :size)
  (def :int64))

(cffi:defcfun (find-bool "ufbx_find_bool_len") :bool
  (props :pointer)
  (name :pointer)
  (name_len :size)
  (def :bool))

(cffi:defcfun (find-prop-concat "ufbx_find_prop_concat") :pointer
  (props :pointer)
  (parts :pointer)
  (num_parts :size))

(cffi:defcfun (get-prop-element "ufbx_get_prop_element") :pointer
  (element :pointer)
  (prop :pointer)
  (type element-type))

(cffi:defcfun (find-element "ufbx_find_element_len") :pointer
  (scene :pointer)
  (type element-type)
  (name :pointer)
  (name_len :size))

(cffi:defcfun (find-node "ufbx_find_node_len") :pointer
  (scene :pointer)
  (name :pointer)
  (name_len :size))

(cffi:defcfun (find-anim-stack "ufbx_find_anim_stack_len") :pointer
  (scene :pointer)
  (name :pointer)
  (name_len :size))

(cffi:defcfun (find-material "ufbx_find_material_len") :pointer
  (scene :pointer)
  (name :pointer)
  (name_len :size))

(cffi:defcfun (find-anim-prop "ufbx_find_anim_prop_len") :pointer
  (layer :pointer)
  (element :pointer)
  (prop :pointer)
  (prop_len :size))

(cffi:defcfun (inflate "ufbx_inflate") :ptrdiff
  (dst :pointer)
  (dst_size :size)
  (input :pointer)
  (retain :pointer))

(cffi:defcfun (open-file "ufbx_open_file") :bool
  (stream :pointer)
  (path :pointer)
  (path_len :size))

(cffi:defcfun (default-open-file "ufbx_default_open_file") :bool
  (user :pointer)
  (stream :pointer)
  (path :pointer)
  (path_len :size)
  (info :pointer))

(cffi:defcfun (open-memory "ufbx_open_memory") :bool
  (stream :pointer)
  (data :pointer)
  (data_size :size)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (evaluate-curve "ufbx_evaluate_curve") :float
  (curve :pointer)
  (time :double)
  (default_value :float))

(cffi:defcfun (evaluate-anim-value-real "ufbx_evaluate_anim_value_real") :float
  (anim_value :pointer)
  (time :double))

(cffi:defcfun (evaluate-blend-weight "ufbx_evaluate_blend_weight") :float
  (anim :pointer)
  (channel :pointer)
  (time :double))

(cffi:defcfun (evaluate-scene "ufbx_evaluate_scene") :pointer
  (scene :pointer)
  (anim :pointer)
  (time :double)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (find-prop-texture "ufbx_find_prop_texture_len") :pointer
  (material :pointer)
  (name :pointer)
  (name_len :size))

(cffi:defcfun (find-shader-texture-input "ufbx_find_shader_texture_input_len") :pointer
  (shader :pointer)
  (name :pointer)
  (name_len :size))

(cffi:defcfun (matrix-determinant "ufbx_matrix_determinant") :float
  (m :pointer))

(cffi:defcfun (add-blend-shape-vertex-offsets "ufbx_add_blend_shape_vertex_offsets") :void
  (shape :pointer)
  (vertices :pointer)
  (num_vertices :size)
  (weight :float))

(cffi:defcfun (add-blend-vertex-offsets "ufbx_add_blend_vertex_offsets") :void
  (blend :pointer)
  (vertices :pointer)
  (num_vertices :size)
  (weight :float))

(cffi:defcfun (evaluate-nurbs-basis "ufbx_evaluate_nurbs_basis") :size
  (basis :pointer)
  (u :float)
  (weights :pointer)
  (num_weights :size)
  (derivatives :pointer)
  (num_derivatives :size))

(cffi:defcfun (tessellate-nurbs-curve "ufbx_tessellate_nurbs_curve") :pointer
  (curve :pointer)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (tessellate-nurbs-surface "ufbx_tessellate_nurbs_surface") :pointer
  (surface :pointer)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (free-line-curve "ufbx_free_line_curve") :void
  (curve :pointer))

(cffi:defcfun (retain-line-curve "ufbx_retain_line_curve") :void
  (curve :pointer))

(cffi:defcfun (compute-topology "ufbx_catch_compute_topology") :void
  (panic :pointer)
  (mesh :pointer)
  (topo :pointer)
  (num_topo :size))

(cffi:defcfun (topo-next-vertex-edge "ufbx_catch_topo_next_vertex_edge") :uint32
  (panic :pointer)
  (topo :pointer)
  (num_topo :size)
  (index :uint32))

(cffi:defcfun (topo-prev-vertex-edge "ufbx_catch_topo_prev_vertex_edge") :uint32
  (panic :pointer)
  (topo :pointer)
  (num_topo :size)
  (index :uint32))

(cffi:defcfun (generate-normal-mapping "ufbx_catch_generate_normal_mapping") :size
  (panic :pointer)
  (mesh :pointer)
  (topo :pointer)
  (num_topo :size)
  (normal_indices :pointer)
  (num_normal_indices :size)
  (assume_smooth :bool))

(cffi:defcfun (compute-normals "ufbx_catch_compute_normals") :void
  (panic :pointer)
  (mesh :pointer)
  (positions :pointer)
  (normal_indices :pointer)
  (num_normal_indices :size)
  (normals :pointer)
  (num_normals :size))

(cffi:defcfun (subdivide-mesh "ufbx_subdivide_mesh") :pointer
  (mesh :pointer)
  (level :size)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (free-mesh "ufbx_free_mesh") :void
  (mesh :pointer))

(cffi:defcfun (retain-mesh "ufbx_retain_mesh") :void
  (mesh :pointer))

(cffi:defcfun (load-geometry-cache "ufbx_load_geometry_cache") :pointer
  (filename :pointer)
  (opts :pointer)
  (error :pointer))

(cffi:defcfun (free-geometry-cache "ufbx_free_geometry_cache") :void
  (cache :pointer))

(cffi:defcfun (retain-geometry-cache "ufbx_retain_geometry_cache") :void
  (cache :pointer))

(cffi:defcfun (read-geometry-cache-real "ufbx_read_geometry_cache_real") :size
  (frame :pointer)
  (data :pointer)
  (num_data :size)
  (opts :pointer))

(cffi:defcfun (sample-geometry-cache-real "ufbx_sample_geometry_cache_real") :size
  (channel :pointer)
  (time :double)
  (data :pointer)
  (num_data :size)
  (opts :pointer))

(cffi:defcfun (read-geometry-cache-vec3 "ufbx_read_geometry_cache_vec3") :size
  (frame :pointer)
  (data :pointer)
  (num_data :size)
  (opts :pointer))

(cffi:defcfun (sample-geometry-cache-vec3 "ufbx_sample_geometry_cache_vec3") :size
  (channel :pointer)
  (time :double)
  (data :pointer)
  (num_data :size)
  (opts :pointer))

(cffi:defcfun (dom-find "ufbx_dom_find_len") :pointer
  (parent :pointer)
  (name :pointer)
  (name_len :size))

(cffi:defcfun (generate-indices "ufbx_generate_indices") :size
  (streams :pointer)
  (num_streams :size)
  (indices :pointer)
  (num_indices :size)
  (allocator :pointer)
  (error :pointer))

;;;;;;;;
(cffi:defcfun (get-vertex-real "ufbx_catch_get_vertex_real") :float
  (panic :pointer)
  (v :pointer)
  (index :size))

(cffi:defcfun (find-vec3 "ufbx_ffi_find_vec3_len") :void
  (retval :pointer)
  (props :pointer)
  (name :pointer)
  (name_len :size)
  (def :pointer))

(cffi:defcfun (find-string "ufbx_ffi_find_string_len") :void
  (retval :pointer)
  (props :pointer)
  (name :pointer)
  (name_len :size)
  (def :pointer))

(cffi:defcfun (find-blob "ufbx_ffi_find_blob_len") :void
  (retval :pointer)
  (props :pointer)
  (name :pointer)
  (name_len :size)
  (def :pointer))

(cffi:defcfun (find-anim-props "ufbx_ffi_find_anim_props") :void
  (retval :pointer)
  (layer :pointer)
  (element :pointer))

(cffi:defcfun (get-compatible-matrix-for-normals "ufbx_ffi_get_compatible_matrix_for_normals") :void
  (retval :pointer)
  (node :pointer))

(cffi:defcfun (evaluate-anim-value-vec2 "ufbx_ffi_evaluate_anim_value_vec2") :void
  (retval :pointer)
  (anim_value :pointer)
  (time :double))

(cffi:defcfun (evaluate-anim-value-vec3 "ufbx_ffi_evaluate_anim_value_vec3") :void
  (retval :pointer)
  (anim_value :pointer)
  (time :double))

(cffi:defcfun (evaluate-prop "ufbx_ffi_evaluate_prop_len") :void
  (retval :pointer)
  (anim :pointer)
  (element :pointer)
  (name :pointer)
  (name_len :size)
  (time :double))

(cffi:defcfun (evaluate-props "ufbx_ffi_evaluate_props") :void
  (retval :pointer)
  (anim :pointer)
  (element :pointer)
  (time :double)
  (buffer :pointer)
  (buffer_size :size))

(cffi:defcfun (evaluate-transform "ufbx_ffi_evaluate_transform") :void
  (retval :pointer)
  (anim :pointer)
  (node :pointer)
  (time :double))

(cffi:defcfun (prepare-prop-overrides "ufbx_ffi_prepare_prop_overrides") :void
  (retval :pointer)
  (overrides :pointer)
  (num_overrides :size))

(cffi:defcfun (quat-mul "ufbx_ffi_quat_mul") :void
  (retval :pointer)
  (a :pointer)
  (b :pointer))

(cffi:defcfun (quat-normalize "ufbx_ffi_quat_normalize") :void
  (retval :pointer)
  (q :pointer))

(cffi:defcfun (quat-fix-antipodal "ufbx_ffi_quat_fix_antipodal") :void
  (retval :pointer)
  (q :pointer)
  (reference :pointer))

(cffi:defcfun (quat-slerp "ufbx_ffi_quat_slerp") :void
  (retval :pointer)
  (a :pointer)
  (b :pointer)
  (time :float))

(cffi:defcfun (quat-rotate-vec3 "ufbx_ffi_quat_rotate_vec3") :void
  (retval :pointer)
  (q :pointer)
  (v :pointer))

(cffi:defcfun (quat-to-euler "ufbx_ffi_quat_to_euler") :void
  (retval :pointer)
  (q :pointer)
  (order rotation-order))

(cffi:defcfun (euler-to-quat "ufbx_ffi_euler_to_quat") :void
  (retval :pointer)
  (v :pointer)
  (order rotation-order))

(cffi:defcfun (matrix-mul "ufbx_ffi_matrix_mul") :void
  (retval :pointer)
  (a :pointer)
  (b :pointer))

(cffi:defcfun (matrix-invert "ufbx_ffi_matrix_invert") :void
  (retval :pointer)
  (m :pointer))

(cffi:defcfun (matrix-for-normals "ufbx_ffi_matrix_for_normals") :void
  (retval :pointer)
  (m :pointer))

(cffi:defcfun (transform-position "ufbx_ffi_transform_position") :void
  (retval :pointer)
  (m :pointer)
  (v :pointer))

(cffi:defcfun (transform-direction "ufbx_ffi_transform_direction") :void
  (retval :pointer)
  (m :pointer)
  (v :pointer))

(cffi:defcfun (transform-to-matrix "ufbx_ffi_transform_to_matrix") :void
  (retval :pointer)
  (transform :pointer))

(cffi:defcfun (matrix-to-transform "ufbx_ffi_matrix_to_transform") :void
  (retval :pointer)
  (m :pointer))

(cffi:defcfun (get-skin-vertex-matrix "ufbx_ffi_get_skin_vertex_matrix") :void
  (retval :pointer)
  (skin :pointer)
  (vertex :size)
  (fallback :pointer))

(cffi:defcfun (get-blend-shape-vertex-offset "ufbx_ffi_get_blend_shape_vertex_offset") :void
  (retval :pointer)
  (shape :pointer)
  (vertex :size))

(cffi:defcfun (get-blend-vertex-offset "ufbx_ffi_get_blend_vertex_offset") :void
  (retval :pointer)
  (blend :pointer)
  (vertex :size))

(cffi:defcfun (evaluate-nurbs-curve "ufbx_ffi_evaluate_nurbs_curve") :void
  (retval :pointer)
  (curve :pointer)
  (u :float))

(cffi:defcfun (evaluate-nurbs-surface "ufbx_ffi_evaluate_nurbs_surface") :void
  (retval :pointer)
  (surface :pointer)
  (u :float)
  (v :float))

(cffi:defcfun (get-weighted-face-normal "ufbx_ffi_get_weighted_face_normal") :void
  (retval :pointer)
  (positions :pointer)
  (face :pointer))

(cffi:defcfun (get-triangulate-face-num-indices "ufbx_ffi_get_triangulate_face_num_indices") :size
  (face :pointer))

(cffi:defcfun (triangulate-face "ufbx_ffi_triangulate_face") :uint32
  (indices :pointer)
  (num_indices :size)
  (mesh :pointer)
  (face :pointer))

(cffi:defcfun (quat-dot "ufbx_ffi_quat_dot") :float
  (a :pointer)
  (b :pointer))

(let ((syms (loop for sym being the symbols of *package*
                  when (eq (symbol-package sym) *package*) 
                  collect sym)))
  (export syms *package*))
