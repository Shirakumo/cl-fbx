#|
 This file is a part of cl-fbx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fbx)

(define-condition fbx-error (error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)
   (description :initarg :description :reader description)
   (info :initarg :info :reader info)
   (stack :initarg :stack :reader stack))
  (:report (lambda (c s) (format s "An FBX error occurred:~%~%[~a]~%~a"
                                 (code c) (message c)))))

(defun fbx-error (error)
  (error 'fbx-error :code (fbx:error-type error)
                    :message (cffi:with-foreign-objects ((string :char 2048))
                               (fbx:format-error string 2048 error)
                               (cffi:foreign-string-to-lisp string))
                    :description (fbx:string-data (fbx:error-description error))
                    :info (cffi:foreign-string-to-lisp (cffi:foreign-slot-pointer error '(:struct fbx:error) 'fbx:info)
                                                       :count (fbx:error-info-length error))
                    :stack (loop for i from 0 below (fbx:error-stack-size error)
                                 for frame = (cffi:foreign-slot-pointer error '(:struct fbx:error) 'fbx:stack)
                                 then (cffi:inc-pointer frame (cffi:foreign-type-size '(:struct fbx:error-frame)))
                                 collect (list i 
                                               (fbx:error-frame-source-line frame) 
                                               (fbx:error-frame-function frame)
                                               (fbx:error-frame-description frame)))))

(defun check-error (error)
  (unless (eql :none (fbx:error-type error))
    (fbx-error error)))

(define-condition fbx-warning (warning)
  ((code :initarg :code :reader code)
   (description :initarg :description :reader description))
  (:report (lambda (c s) (format s "An FBX warning was signalled:~%~%[~a]~%~a"
                                 (code c) (description c)))))

(defun fbx-warning (warning)
  (warn 'fbx-warning :code (fbx:warning-type warning)
                     :description (fbx:string-data (fbx:warning-description warning))))

(define-condition fbx-panic (error)
  ((message :initarg :message :reader message))
  (:report (lambda (c s) (format s "An FBX panic was signalled:~%~%~a"
                                 (message c)))))

(defun fbx-panic (panic)
  (error 'fbx-panic :message (cffi:foreign-string-to-lisp (cffi:foreign-slot-pointer (handle panic) '(:struct fbx:panic) 'fbx:message)
                                                          :count (fbx:panic-message-length (handle panic)))))

(defun check-panic (panic)
  (when (fbx:panic-did-panic panic)
    (fbx-panic panic)))

(define-condition inflate-error (error)
  ((code :initarg :code :reader code))
  (:report (lambda (c s) (format s "An inflate error was signalled:~%~a"
                                 (description c)))))

(defmethod description ((error inflate-error))
  (case (code error)
    (-1 "Bad compression method (ZLIB header)")
    (-2 "Requires dictionary (ZLIB header)")
    (-3 "Bad FCHECK (ZLIB header)")
    (-4 "Bad NLEN (Uncompressed LEN != ~NLEN)")
    (-5 "Uncompressed source overflow")
    (-6 "Uncompressed destination overflow")
    (-7 "Bad block type")
    (-8 "Truncated checksum (deprecated, reported as -9)")
    (-9 "Checksum mismatch")
    (-10 "Literal destination overflow")
    (-11 "Bad distance code or distance of (30..31)")
    (-12 "Match out of bounds")
    (-13 "Bad lit/length code")
    (-14 "Codelen Huffman Overfull")
    (-15 "Codelen Huffman Underfull")
    (-16 "Litlen Huffman Overfull")
    (-17 "Litlen Huffman Underfull")
    (-18 "Litlen Huffman Repeat 16 overflow")
    (-19 "Litlen Huffman Repeat 17 overflow")
    (-20 "Litlen Huffman Repeat 18 overflow")
    (-21 "Litlen Huffman Bad length code")
    (-22 "Distance Huffman Overfull")
    (-23 "Distance Huffman Underfull")
    (-24 "Distance Huffman Repeat 16 overflow")
    (-25 "Distance Huffman Repeat 17 overflow")
    (-26 "Distance Huffman Repeat 18 overflow")
    (-27 "Distance Huffman Bad length code")
    (-28 "Cancelled")
    (-29 "Invalid ufbx_inflate_input.internal_fast_bits value")
    (T "Unknown")))

(defun inflate-error (code)
  (error 'inflate-error :code code))
