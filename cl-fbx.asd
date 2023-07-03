(asdf:defsystem cl-fbx
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Bindings to ufbx, a simple and free FBX model decoding library"
  :homepage "https://Shirakumo.github.io/cl-fbx/"
  :bug-tracker "https://github.com/Shirakumo/cl-fbx/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-fbx.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "conditions")
               (:file "wrapper")
               (:file "file")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :trivial-extensible-sequences
               :float-features
               :static-vectors
               :documentation-utils))
