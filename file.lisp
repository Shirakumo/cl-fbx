#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fbx)

(defun init ()
  (unless (cffi:foreign-library-loaded-p 'fbx:libfbx)
    (cffi:load-foreign-library 'fbx:libfbx)))

(defun shutdown ()
  (when (cffi:foreign-library-loaded-p 'fbx:libfbx)
    (cffi:close-foreign-library 'fbx:libfbx)))

(defmacro with-file ((file source &rest args) &body body)
  `(with-freeing ((,file (parse ,source ,@args)))
     ,@body))

(defclass fbx-file (scene)
  ((source :initarg :source :initform NIL :accessor source)))

(defmethod print-object ((file fbx-file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~a" (source file))))

(defmethod free ((file fbx-file))
  (when (handle file)
    (fbx:free-scene (handle file))
    (setf (handle file) NIL)))

(defun parse (source &rest args)
  (init)
  (cffi:with-foreign-objects ((error '(:struct fbx:error)))
    (let* ((opts (apply #'make-instance 'load-opts args))
           (result (apply #'%parse source (handle opts) error args)))
      (unwind-protect (check-error error)
        (free opts))
      result)))

(defgeneric %parse (source opts error &key &allow-other-keys))

(defmethod %parse ((source string) opts error &key)
  (make-instance 'fbx-file :source source :handle (fbx:load-file source opts error)))

(defmethod %parse ((source pathname) opts error &key)
  (make-instance 'fbx-file :source source :handle (fbx:load-file (namestring source) opts error)))

(defmethod %parse ((source vector) opts error &rest args &key static-vector)
  (check-type source (vector (unsigned-byte 8)))
  (if static-vector
      (apply #'%parse (static-vectors:static-vector-pointer source) opts error :data-size (length source) args)
      (let ((ptr (cffi:foreign-alloc :uint8 :count (length source) :initial-contents source)))
        (apply #'%parse ptr opts error :data-size (length source) :deallocate T args))))

(defclass fbx-file-pointer (fbx-file)
  ((deallocate-p :initarg :deallocate-p :accessor deallocate-p)
   (pointer :initarg :pointer :accessor pointer)))

(defmethod free :after ((file fbx-file-pointer))
  (when (and (deallocate-p file) (pointer file))
    (cffi:foreign-free (pointer file))
    (setf (pointer file) NIL)))

(defmethod %parse (source opts error &key data-size deallocate)
  (etypecase source
    (cffi:foreign-pointer
     (make-instance 'fbx-file-pointer :source source
                                      :handle (fbx:load-memory source data-size opts error)
                                      :deallocate-p deallocate))))

(defclass fbx-file-stream (fbx-file)
  ((stream :initarg :stream :accessor %stream)
   (buffer :initarg :buffer :initform (make-array 4096 :element-type '(unsigned-byte 8)) :accessor buffer)
   (stream-struct :initarg :stream-struct :accessor stream-struct)))

(defmethod free :after ((file fbx-file-stream))
  (when (stream-struct file)
    (setf (global-pointer (stream-struct file)) NIL)
    (cffi:foreign-free (stream-struct file))
    (setf (stream-struct file) NIL))
  (when (%stream file)
    (close (%stream file))
    (setf (%stream file) NIL)))

(defmethod %parse ((source stream) opts error &key)
  (let ((stream (cffi:foreign-alloc '(:struct fbx:stream))))
    (setf (fbx:stream-read-fn stream) (cffi:callback stream-read-cb))
    (setf (fbx:stream-skip-fn stream) (cffi:callback stream-skip-cb))
    (setf (fbx:stream-close-fn stream) (cffi:callback stream-close-cb))
    (setf (fbx:stream-user stream) stream)
    (make-instance 'fbx-file-stream :source source :handle (fbx:load-stream stream opts error) :stream source :stream-struct stream)))

(cffi:defcallback stream-read-cb :size ((user :pointer) (data :pointer) (size :size))
  (with-ptr-resolve (file user)
    (let* ((buffer (buffer file))
           (read (read-sequence buffer (%stream file) :end (min size (length buffer)))))
      (cffi:with-pointer-to-vector-data (ptr buffer)
        (static-vectors:replace-foreign-memory data ptr read))
      read)))

(cffi:defcallback stream-skip-cb :bool ((user :pointer) (size :size))
  (with-ptr-resolve (file user)
    (let ((stream (%stream file)))
      (etypecase stream
        (file-stream
         (file-position stream (+ (file-position stream) size)))
        (stream
         (let ((buffer (buffer file)))
           (loop until (<= size 0)
                 for read = (read-sequence buffer stream :end (min (length buffer) size))
                 do (decf size read))
           T))))))

(cffi:defcallback stream-close-cb :void ((user :pointer))
  (with-ptr-resolve (file user)
    (close (%stream file))))

(defmethod load-cache ((path pathname) &rest args)
  (apply #'load-cache (namestring path) args))

(defmethod load-cache ((path string) &rest args)
  (cffi:with-foreign-objects ((error '(:struct fbx:error)))
    (let* ((opts (apply #'make-instance 'geometry-cache-opts args))
           (handle (fbx:load-geometry-cache path opts error)))
      (unwind-protect (check-error error)
        (free opts))
      (make-instance 'geometry-cache :handle handle))))
