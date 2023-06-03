#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-fbx
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to ufbx, a simple and free FBX model decoding library"
  :homepage "https://Shirakumo.github.io/cl-fbx/"
  :bug-tracker "https://github.com/Shirakumo/cl-fbx/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-fbx.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :static-vectors
               :documentation-utils))
