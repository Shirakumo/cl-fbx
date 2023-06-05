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
    "")
  
  (function memory-limit
    "") 
  
  (function allocation-limit
    "") 
  
  (function huge-threshold
    "") 
  
  (function max-chunk-size
    "") 
  
  (function free
    "") 
  
  (function allocate
    "") 
  
  (function reallocate
    "") 
  
  (function deallocate
    "") 
  
  (function update
    "") 
  
  (function with-freeing
    "") 
  
  (type wrapper
    "") 
  
  (function handle
    "") 
  
  (function foreign-type
    "") 
  
  (type foreign-vector
    "") 
  
  (function lisp-type
    "") 
  
  (function wrap-foreign-vector
    "") 
  
  (function make-foreign-vector
    "") 
  
  (function progress-cb
    "") 
  
  (function open-file-cb
    "") 
  
  (function close-memory-cb
    "") 
  
  (function find
    "") 
  
  (function get-element
    "") 
  
  (function evaluate
    "") 
  
  (function add-offsets
    "") 
  
  (function tessellate
    "") 
  
  (function compute-topology
    "") 
  
  (function next-vertex
    "") 
  
  (function prev-vertex
    "") 
  
  (function generate-normal-mapping
    "") 
  
  (function compute-normals
    "") 
  
  (function subdivide-mesh
    "") 
  
  (function inflate
    "") 
  
  (function read-cache
    "") 
  
  (function sample-cache
    "") 
  
  (function normal-matrix
    "") 
  
  (function skin-vertex-matrix
    "") 
  
  (function blend-vertex-offset
    "") 
  
  (function weighted-face-normal
    "") 
  
  (function triangulate-face
    ""))

