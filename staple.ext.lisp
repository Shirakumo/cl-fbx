(asdf:load-system :staple-markless)

(defpackage "fbx-docs"
  (:use #:cl)
  (:local-nicknames
   (#:fbx #:org.shirakumo.fraf.fbx)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "fbx-docs")))

(defmethod staple:packages ((system (eql (asdf:find-system :cl-fbx))))
  (list (find-package '#:org.shirakumo.fraf.fbx)))

(defmethod staple:page-type ((system (eql (asdf:find-system :cl-fbx))))
  'page*)
