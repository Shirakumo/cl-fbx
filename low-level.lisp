#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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

