#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fbx)

;; conditions.lisp
(docs:define-docs
  (type fbx-error
    "Error signalled on some FBX operations.

See CODE
See MESSAGE
See DESCRIPTION
See INFO
See STACK")

  (function code
    "The internal error code of fbx conditions.

See FBX-ERROR (type)
See FBX-WARNING (type)
See INFLATE-ERROR (type)")

  (function message
    "Returns the message associated with the error.

See FBX-ERROR (type)
See FBX-PANIC (type)")

  (function description
    "Returns the description of the condition.

See FBX-ERROR (type)
See FBX-WARNING (type)
See INFLATE-ERROR (type)")

  (function info
    "Returns additional information about the condition.

See FBX-ERROR (type)")

  (function stack
    "Returns the stack around the error.

This is a list of frames, where each frame is structured as follows:
  (INDEX SOURCE-LINE FUNCTION-NAME DESCRIPTION)

See FBX-ERROR (type)")

  (type fbx-warning
    "Warning signalled on some FBX operations.

See CODE
See DESCRIPTION")

  (type fbx-panic
    "Error signalled on some FBX operations.

See MESSAGE")

  (type inflate-error
    "Error signalled when an INFLATE invocation fails.

See CODE
See DESCRIPTION
See INFLATE"))

;; file.lisp
(docs:define-docs
  (type fbx-file
    "Representation of an FBX file.

Note that this does not necessarily represent a file loaded from
disk.

This is also a SCENE instance.

You must FREE the file when you are done with it. Accessing any fields
of the file, or any derived WRAPPER instances after freeing it leads
to undefined behaviour.

You should call PARSE with an appropriate file source to create
instances of this class.

See FREE
See SCENE
See PARSE
See SOURCE")

  (function source
    "Returns the source the file was created from.

See FBX-FILE (type)")

  (function parse
    "Parses an FBX file from the given source.

SOURCE may be one of:
  STRING   --- A native namestring
  PATHNAME --- A file designator
  STREAM   --- An octet-stream
               When the file is FREEd, the stream is closed as well.
  VECTOR   --- An octet-vector
               You may pass :STATIC-VECTOR T if the vector is a
               static-vector, in which case the vector's data is used
               directly. Otherwise the data is copied to a C memory
               region.
  CFFI:FOREIGN-POINTER --- A pointer to the memory area
               You must also pass :DATA-SIZE designating the number
               of octets the memory region encompasses. You may also
               pass :DEALLOCATE T to hand over the ownership of the
               memory region and cause FREE of the result to free the
               memory region as well.

Returns an FBX-FILE if successful and signals an FBX-ERROR otherwise.

Please see LOAD-OPTS for the available arguments. Any LOAD-OPTS slot
may be specified as a keyword argument.

See FBX-FILE (type)
See FBX-ERROR (type)
See LOAD-OPTS (type)")

  (function load-cache
    "Load a geometry cache.

SOURCE may be one of:
  STRING   --- A native namestring
  PATHNAME --- A file desgnator

Returns a GEOMETRY-CACHE if successful and signals an FBX-ERROR
otherwise.

Please see GEOMETRY-CACHE-OPTS for the available arguments. Any
GEOMETRY-CACHE-OPTS slot may be specified as a keyword argument.

See GEOMETRY-CACHE (type)
See FBX-ERROR (type)
See GEOMETRY-CACHE-OPTS (type)"))

;; wrapper.lisp
(docs:define-docs
  (type allocator
    "Representation of an allocation manager.

You should subclass this and implement the following:
  FREE
  ALLOCATE
  REALLOCATE
  DEALLOCATE

See FREE
See ALLOCATE
See REALLOCATE
See DEALLOCATE
See MEMORY-LIMIT
See ALLOCATION-LIMIT
See HUGE-THRESHOLD
See MAX-CHUNK-SIZE
See RESULT-ALLOCATOR
See TEMP-ALLOCATOR")

  (function memory-limit
    "Accesses the maximum addressable number of bytes the allocator can hand out.

See ALLOCATOR (type)")

  (function allocation-limit
    "Accesses the maximum number of allocations that can be performed.

See ALLOCATOR (type)")

  (function huge-threshold
    "Accesses the threshold beyond which ufbx will use individual allocations per element rather than batching them together.

See ALLOCATOR (type)")

  (function max-chunk-size
    "Accesses the maximum number of bytes the allocator can hand out in one allocation.

See ALLOCATOR (type)")

  (function free
    "Frees all foreign resources associated with the object.

After calling FREE, you should not manipulate the object in any
way. However, it is safe to call FREE multiple times.

See ALLOCATOR (type)
See WRAPPER (type)
See FOREIGN-VECTOR (type)")

  (function allocate
    "Allocate a memory region of the given size.

Should return a CFFI:FOREIGN-POINTER.
A failure can be indicated by returning a null pointer.

See ALLOCATOR (type)")

  (function reallocate
    "Reallocate a memory region to the given size.

Should return a CFFI:FOREIGN-POINTER.
A failure can be indicated by returning a null pointer.

See ALLOCATOR (type)")

  (function deallocate
    "Free a memory region allocated by the allocator.

See ALLOCATOR (type)")

  (function update
    "Update the object's fields using a plist.

This is equivalent to calling SETF on each field in turn, just using
keyword arguments.

You may also instead use REINITIALIZE-INSTANCE to achieve the same.

See WRAPPER (type)")

  (function with-freeing
    "LET with additional freeing semantics.

Upon exit of the BODY for any reason, FREE is called on each value of
each binding in reverse order of the value forms.

See FREE
See WRAPPER (type)")

  (type wrapper
    "Representation of a foreign object.

Unless specifically stated, or you allocated it yourself, you should
*not* call FREE on a wrapper instance, as the pointer it represents is
probably not yours to free.

See HANDLE
See FREE
See FOREIGN-TYPE")

  (function handle
    "Accessor to the underlying C pointer the object represents.

See WRAPPER (type)
See FOREIGN-VECTOR (type)")

  (function foreign-type
    "Returns the foreign type specifier of the thing the object represents.

See WRAPPER (type)
See FOREIGN-VECTOR (type)")

  (type foreign-vector
    "Representation of a vector of foreign values.

This can be accessed using standard sequence functions.

See CL:ELT
See CL:LENGTH
See LISP-TYPE
See FOREIGN-TYPE
See HANDLE")

  (function lisp-type
    "Returns the lisp type specifier of the elements represented by the vector.

See FOREIGN-VECTOR (type)")

  (function wrap-foreign-vector
    "Wrap the handle in a FOREIGN-VECTOR instance.

See FOREIGN-VECTOR (type)")

  (function make-foreign-vector
    "Create a FOREIGN-VECTOR that can hold SIZE number of elements of FOREIGN-TYPE.

Depending on whether the foreign-type instances are immediates or not,
the resulting vector will be initialised with empty instances of the
type or null pointers.

See FOREIGN-VECTOR (type)")

  (function (setf progress-cb)
    "Sets the progress callback.

Should be a function of two arguments:
  CURRENT
  MAX")

  (function (setf open-file-cb)
    "Sets the open-file callback.

Should be a function of four arguments:
  STREAM
  PATH
  FILE-TYPE
  ALLOCATOR")

  (function (setf close-memory-cb)
    "Sets the close-memory callback.

Should be a function of two arguments:
  DATA
  SIZE")

  (function find
    "Attempts to find a thing within a container.

NAME should usually be a string naming the element to find.
CONTAINER may be one of:
  PROPS      --- You should pass a TYPE argument to identify what to find:
    PROP
    VEC3     --- The vector should be a 3-element vector.
    STRING
    BLOB
    INTEGER
    REAL
    BOOLEAN
  SCENE      --- You should pass a TYPE argument to identify what to find:
    NODE
    ANIM-STACK
    MATERIAL
    any element type name
  MATERIAL
  SHADER
  ANIM-LAYER --- You should also pass an ELEMENT argument designating
                 the ELEMENT instance to search through.
  DOM-NODE

Most of these also accept a DEFAULT which is returned if the requested
element cannot be found.")

  (function get-element
    "Gets the associated element of the requested type.

See ELEMENT (type)
See PROP (type)")

  (function evaluate
    "Evaluate a representation for a value.

The representation may be one of:
  ANIM-CURVE
   :DEFAULT
  ANIM-VALUE  --- You should pass a TYPE argument to identify the value:
   REAL
   VEC2
   VEC3
  ANIM        --- You should pass a TYPE argument to identify the value:
   REAL
   PROP       --- You should also pass:
     TARGET   --- The property to write the value into. If not passed,
                  a property is allocated and returned. You must free
                  it when no longer needed.
     NAME     --- The name of the property to evaluate.
   TRANSFORM  --- You may also pass:
     TARGET   --- The transform to write the value into. If not passed,
                  a transform is allocated and returned. You must free
                  it when no longer needed.
  SCENE       --- You should also pass:
   ANIMATION  --- The animation according to which to evaluate the
                  scene.
   any slot value for EVALUATE-OPTS
                  A fresh SCENE instance is returned. Note that it is
                  a shallow copy and will reference data from the
                  original scene.
  NURBS-BASIS --- You may also pass:
   WEIGHTS    --- A vector of single-floats
   DERIVATIVES -- A vector of single-floats
                  If not passed they will be allocated for you.
  NURBS-CURVE --- You may also pass:
   POINT      --- A CURVE-POINT. If not passed it will be allocated
                  and returned for you.
  NURBS-SURFACE --- You may also pass:
   POINT      --- A CURVE-POINT. If not passed it will be allocated
                  and returned for you.
                    Additionally, the time argument must be a cons
                    identifying the U and V coordinates to evaluate.
The second argument should be the place / time at which to evaluate
the representation.

See EVALUATE-OPTS (type)")

  (function add-offsets
    "Add weighted vertices to the deformer.

VERTICES should be a vector of SINGLE-FLOATs where each vector is
packed as three floats.

See BLEND-DEFORMER (type)")

  (function tessellate
    "Tessellates the given nurbs object.

May be one of:
  NURBS-CURVE
   Arguments may be any slot for TESSELLATE-CURVE-OPTS.
   Returns a LINE-CURVE instance.
  NURBS-SURFACE
   Arguments may be any slot for TESSELLATE-SURFACE-OPTS.
   Returns a MESH instance.

See NURBS-CURVE (type)
See LINE-CURVE (type)
See NURBS-SURFACE (type)
See MESH (type)
See TESSELLATE-CURVE-OPTS (type)
See TESSELLATE-SURFACE-OPTS (type)")

  (function compute-topology
    "Computes the topology of a mesh.

TOPO should be a FOREIGN-VECTOR representing TOPO-EDGE instances. If
not passed, a vector will be allocated and returned for you.

May signal an error of type FBX-PANIC.

See FBX-PANIC (type)
See MESH (type)
See FOREIGN-VECTOR (type)
See TOPO-EDGE (type)")

  (function next-vertex
    "Returns the index of the next vertex after the one of the given index.

May signal an error of type FBX-PANIC.

See FBX-PANIC (type)")

  (function prev-vertex
    "Returns the index of the previous vertex before the one of the given index.

May signal an error of type FBX-PANIC.

See FBX-PANIC (type)")

  (function generate-normal-mapping
    "Generate the normal mapping for a topology.

Returns the length of and the normal indices array.

If no NORMAL-INDICES array is passed, one is created for you.

TOPO should be a FOREIGN-VECTOR as returned by COMPUTE-TOPOLOGY

See COMPUTE-TOPOLOGY
See MESH (type)")

  (function compute-normals
    "Compute the normals for a mesh's vertices.

POSITIONS should be a single-float vector of packed vec3s.
NORMAL-INDICES should be an (unsigned-byte 32) vector.
NORMALS may be a single-float vector of packed vec3s.

If NORMALS is not passed, a vector is allocated and returned for you.

Returns two values: the number of normals and the normals vector.

See MESH (type)")

  (function subdivide-mesh
    "Subdivides the mesh by the given number of levels.

You may pass arguments as there are slots for SUBDIVIDE-OPTS.

Returns a new MESH instance.

May signal an error of type FBX-ERROR.

See SUBDIVIDE-OPTS (type)
See FBX-ERROR (type)
See MESH (type)")

  (function inflate
    "Inflate compressed data.

TARGET should be an octet vector to write the decompressed data into.

Returns the number of octets written to the target vector.

May signal an error of type INFLATE-ERROR.

See INFLATE-INPUT (type)
See INFLATE-ERROR (type)")

  (function read-cache
    "Read a value from the geometry cache.

TYPE may be VEC3 or REAL.
You may pass options as there are slots for GEOMETRY-CACHE-DATA-OPTS.

See GEOMETRY-CACHE (type)
See GEOMETRY-CACHE-DATA-OPTS (type)")

  (function sample-cache
    "Sample a value from the geometry cache at the given time.

TYPE may be VEC3 or REAL.
You may pass options as there are slots for GEOMETRY-CACHE-DATA-OPTS.

See GEOMETRY-CACHE (type)
See GEOMETRY-CACHE-DATA-OPTS (type)")

  (function normal-matrix
    "Compute a normal matrix for the given node.

MATRIX should be a 16-element single-float array to hold the matrix
components. If it is not passed, a vector is allocated and returned
for you.

See NODE (type)")

  (function skin-vertex-matrix
    "Compute a skinning matrix for the given skin and vertex.

MATRIX should be a 16-element single-float array to hold the matrix
components. If it is not passed, a vector is allocated and returned
for you.

See SKIN-DEFORMER (type)")

  (function blend-vertex-offset
    "Compute the offset for the given blend shape and vertex.

VERTEX should be a 3-element single-float array to hold the vector
components. If it is not passed, a vector is allocated and returned
for you.

See BLEND-SHAPE (type)")

  (function weighted-face-normal
    "Compute the weighted face normal for the given array of vertices.

NORMAL should be a 3-element single-float array to hold the normal vector
components. If it is not passed, a vector is allocated and returned
for you.

See FACE (type)")

  (function triangulate-face
    "Compute the triangulation of a face.

INDICES should be an (unsigned-byte 32) array to hold the triangulation
indices. If it is not passed, a vector is allocated and returned
for you.

See MESH (type)
See FACE (type)"))
