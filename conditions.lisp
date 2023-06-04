#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fbx)

(define-condition fbx-error (error)
  ((handle :initarg :handle :reader handle))
  (:report (lambda (c s) (format s "An FBX error occurred:~%~%[~a]~%~a"
                                 (code c) (message c)))))

(defmethod message ((error fbx-error))
  (cffi:with-foreign-objects ((string :char 2048))
    (fbx:format-error string 2048 (handle error))
    (cffi:foreign-string-to-lisp string)))

(defmethod code ((error fbx-error))
  (fbx:error-type (handle error)))

(defmethod description ((error fbx-error))
  (fbx:string-data (fbx:description (handle error))))

(defmethod info ((error fbx-error))
  (cffi:foreign-string-to-lisp (cffi:foreign-slot-pointer (handle error) '(:struct fbx:error) 'fbx:info)
                               :count (fbx:error-info-length (handle error))))

(defmethod stack ((error fbx-error))
  (loop for i from 0 below (fbx:error-stack-size (handle error))
        for frame = (cffi:foreign-slot-pointer (handle error) '(:struct fbx:error) 'fbx:stack)
        then (cffi:inc-pointer frame (cffi:foreign-type-size '(:struct fbx:error-frame)))
        collect (list i 
                      (fbx:error-frame-source-line frame) 
                      (fbx:error-frame-function frame)
                      (fbx:error-frame-description frame))))

(defun check-error (error)
  (unless (eql :none (fbx:error-type error))
    (error 'fbx-error :handle error)))

(define-condition fbx-warning (warning)
  ((handle :initarg :handle :reader handle))
  (:report (lambda (c s) (format s "An FBX warning was signalled:~%~%[~a]~%~a"
                                 (code c) (description c)))))

(defmethod code ((warning fbx-warning))
  (fbx:warning-type (handle warning)))

(defmethod description ((warning fbx-warning))
  (fbx:string-data (fbx:warning-description (handle warning))))

(define-condition fbx-panic (error)
  ((handle :initarg :handle :reader handle))
  (:report (lambda (c s) (format s "An FBX panic was signalled:~%~%~a"
                                 (message c)))))

(defmethod message ((panic fbx-panic))
  (cffi:foreign-string-to-lisp (cffi:foreign-slot-pointer (handle panic) '(:struct fbx:panic) 'fbx:message)
                               :count (fbx:panic-message-length (handle panic))))

(defun check-panic (panic)
  (when (fbx:panic-did-panic panic)
    (error 'fbx-panic :handle panic)))
