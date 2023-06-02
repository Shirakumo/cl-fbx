#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.fbx.cffi
  (:use #:cl)
  (:shadow)
  (:export
   #:libfbx))

(defpackage #:org.shirakumo.fraf.fbx
  (:use #:cl)
  (:local-nicknames
   (#:fbx #:org.shirakumo.fraf.fbx.cffi))
  (:shadow)
  (:export
   #:fbx-error))
