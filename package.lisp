#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:org.shirakumo.fraf.fbx.cffi)
    (make-package '#:org.shirakumo.fraf.fbx.cffi :use '(#:cl)))
  (shadow '(cl:type cl:string cl:list cl:character cl:warning cl:stream cl:error) '#:org.shirakumo.fraf.fbx.cffi))

(defpackage #:org.shirakumo.fraf.fbx
  (:use #:cl)
  (:local-nicknames
   (#:fbx #:org.shirakumo.fraf.fbx.cffi)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:shadow
   #:values #:time #:character #:position #:find)
  (:export
   #:fbx-error))
