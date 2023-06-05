#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:org.shirakumo.fraf.fbx.cffi)
    (defpackage #:org.shirakumo.fraf.fbx.cffi
      (:use #:cl)
      (:shadow #:type #:string #:list #:character #:warning #:stream #:error)))

  (unless (find-package '#:org.shirakumo.fraf.fbx)
    (defpackage #:org.shirakumo.fraf.fbx
      (:use #:cl)
      (:local-nicknames
       (#:fbx #:org.shirakumo.fraf.fbx.cffi)
       (#:sequences #:org.shirakumo.trivial-extensible-sequences))
      (:shadow
       #:values #:time #:character #:position #:find)
      ;; NOTE: bunch of auto-exports
      ;; conditions.lisp
      (:export
       #:fbx-error
       #:code
       #:message
       #:description
       #:info
       #:stack
       #:fbx-warning
       #:code
       #:description
       #:fbx-panic
       #:message
       #:inflate-error
       #:code)
      ;; file.lisp
      (:export
       #:fbx-file
       #:source
       #:parse
       #:load-cache)
      ;; wrapper.lisp
      (:export
       #:allocator
       #:memory-limit
       #:allocation-limit
       #:huge-threshold
       #:max-chunk-size
       #:free
       #:allocate
       #:reallocate
       #:deallocate
       #:update
       #:with-freeing
       #:wrapper
       #:handle
       #:foreign-type
       #:foreign-vector
       #:lisp-type
       #:data
       #:wrap-foreign-vector
       #:make-foreign-vector
       #:data
       #:progress-cb
       #:open-file-cb
       #:close-memory-cb
       #:temp-allocator
       #:result-allocator
       #:find
       #:get-element
       #:evaluate
       #:add-offsets
       #:tessellate
       #:compute-topology
       #:next-vertex
       #:prev-vertex
       #:generate-normal-mapping
       #:compute-normals
       #:subdivide-mesh
       #:inflate
       #:read-cache
       #:sample-cache
       #:normal-matrix
       #:skin-vertex-matrix
       #:blend-vertex-offset
       #:weighted-face-normal
       #:triangulate-face))))
