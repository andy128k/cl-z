;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage cl-z
  (:use :common-lisp))

(in-package :cl-z)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library zlib
    (:unix (:or "libz.so.1" "libz.so"))
    (:windows (:or "zlib1.dll" "zlib.dll"))
    (t (:default "libz"))))

(cffi:use-foreign-library zlib)

(cffi:defcstruct z-stream
    "zlib stream"
  (next-in :pointer)  ; next input byte
  (avail-in :uint)    ; number of bytes available at next-in
  (total-in :ulong)   ; total nb of input bytes read so far

  (next-out :pointer) ; next output byte should be put there
  (avail-out :uint)   ; remaining free space at next_out
  (total-out :ulong)  ; total nb of bytes output so far

  (msg :pointer)      ; last error message, NULL if no error
  (state :pointer)    ; not visible by applications

  (zalloc :pointer)   ; used to allocate the internal state
  (zfree :pointer)    ; used to free the internal state
  (opaque :pointer)   ; private data object passed to zalloc and zfree

  (data-type :int)    ; best guess about the data type: binary or text
  (adler :ulong)      ; adler32 value of the uncompressed data
  (reserved :ulong))  ; reserved for future use

(cffi:defcenum z-error
    "Return codes for the compression/decompression functions. Negative
     values are errors, positive values are used for special but normal events."
  (:ok 0)
  (:stream-end 1)
  (:need-dict 2)
  (:errno -1)
  (:stream-error -2)
  (:data-error -3)
  (:mem-error -4)
  (:buf-error -5)
  (:version-error -6))

(defparameter +zlib-version+ "1.2.3.4")

(cffi:defcenum z-flush
    "Allowed flush values; see deflate() and inflate() below for details"
  (:no-flush 0)
  (:partial-flush 1)
  (:sync-flush 2)
  (:full-flush 3)
  (:finish 4)
  (:block 5)
  (:trees 6))

(cffi:defcfun (deflate-init "deflateInit_") z-error
  (stream z-stream)
  (level :int)
  (version :string)
  (stream-size :int))

(cffi:defcfun (deflate "deflate") z-error
  (stream z-stream)
  (flush z-flush))

(cffi:defcfun (deflate-end "deflateEnd") z-error
  (stream z-stream))

(cffi:defcfun (inflate-init "inflateInit_") z-error
  (stream z-stream)
  (version :string)
  (stream-size :int))

(cffi:defcfun (inflate "inflate") z-error
  (stream z-stream)
  (flush z-flush))

(cffi:defcfun (inflate-end "inflateEnd") z-error
  (stream z-stream))

(defclass vector-input ()
  ((vector :initarg :vector :accessor vector-input-vector)
   (pos :initarg :start :accessor vector-input-pos)
   (end :initarg :end :accessor vector-input-end)))

(defgeneric read-to-foreign-buffer (stream buffer size))

(defmethod read-to-foreign-buffer ((stream vector-input) buffer size)
  (loop
     for i from 0 below size
     while (< (vector-input-pos stream) (vector-input-end stream))
     do
       (setf (cffi:mem-aref buffer :uint8 i)
	     (aref (vector-input-vector stream) (vector-input-pos stream)))
       (incf (vector-input-pos stream))
     finally (return i)))

(defmethod read-to-foreign-buffer ((stream stream) buffer size)
  (let* ((tmp-buffer (make-array (list size) :element-type '(unsigned-byte 8)))
	 (end (read-sequence tmp-buffer stream)))

    (loop
       for i from 0 below end
       do (setf (cffi:mem-aref buffer :uint8 i) (aref tmp-buffer i)))

    end))

(defgeneric write-foreign-buffer (stream buffer size))

(defmethod write-foreign-buffer ((stream vector) buffer size)
  (loop
     for i from 0 below size
     do (vector-push-extend (cffi:mem-aref buffer :uint8 i) stream)))

(defmethod write-foreign-buffer ((stream stream) buffer size)
  (let ((tmp-buffer (make-array (list size) :element-type '(unsigned-byte 8))))
    (loop
       for i from 0 below size
       do (setf (aref tmp-buffer i) (cffi:mem-aref buffer :uint8 i)))
    (write-sequence tmp-buffer stream)))

(defun process (input-stream output-stream
		init-func
		proc-func
		end-func)

  (cffi:with-foreign-objects ((stream 'z-stream)
			      (input-buffer :uint8 16384)
			      (output-buffer :uint8 16384))

    (setf (cffi:foreign-slot-value stream 'z-stream 'zalloc) (cffi:null-pointer)
	  (cffi:foreign-slot-value stream 'z-stream 'zfree) (cffi:null-pointer)
	  (cffi:foreign-slot-value stream 'z-stream 'opaque) (cffi:null-pointer))

    (let ((err (funcall init-func stream +zlib-version+ (cffi:foreign-type-size 'z-stream))))
      (unless (eq err :ok)
	(error "~A" err)))

    (loop
       (let ((end (read-to-foreign-buffer input-stream input-buffer 16384)))
	 (when (zerop end)
	   (return))

	 (setf (cffi:foreign-slot-value stream 'z-stream 'next-in) input-buffer
	       (cffi:foreign-slot-value stream 'z-stream 'avail-in) end)

	 (loop
	    while (/= 0 (cffi:foreign-slot-value stream 'z-stream 'avail-in))
	    do

	      (setf (cffi:foreign-slot-value stream 'z-stream 'next-out) output-buffer
		    (cffi:foreign-slot-value stream 'z-stream 'avail-out) 16384)

	      (let ((err (funcall proc-func stream :no-flush)))
		(write-foreign-buffer output-stream
				      output-buffer
				      (-
				       (cffi:pointer-address (cffi:foreign-slot-value stream 'z-stream 'next-out))
				       (cffi:pointer-address output-buffer)))

		(unless (eq err :ok)
		  (return))))))

    (loop
       (setf (cffi:foreign-slot-value stream 'z-stream 'next-out) output-buffer
	     (cffi:foreign-slot-value stream 'z-stream 'avail-out) 16384)

       (let ((err (funcall proc-func stream :finish)))
	 (write-foreign-buffer output-stream
			       output-buffer
			       (-
				(cffi:pointer-address (cffi:foreign-slot-value stream 'z-stream 'next-out))
				(cffi:pointer-address output-buffer)))
	 (when (eq err :stream-end)
	   (return))))

    (funcall end-func stream)))

(defun compress-stream (input-stream output-stream &key (compression-level -1))
  (process input-stream
	   output-stream
	   (lambda (stream version size) (deflate-init stream compression-level version size))
	   #'deflate
	   #'deflate-end))

(export 'compress-stream)

(defun uncompress-stream (input-stream output-stream)
  (process input-stream
	   output-stream
	   #'inflate-init
	   #'inflate
	   #'inflate-end))

(export 'uncompress-stream)

(defun compress-vector (input &key (compression-level -1) (start 0) end)
  (let ((out (make-array 0 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8)))
	(in (make-instance 'vector-input :vector input :start start :end (or end (length input)))))
    (compress-stream in out :compression-level compression-level)
    out))

(export 'compress-vector)

(defun uncompress-vector (input &key (start 0) end)
  (let ((out (make-array 0 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8)))
	(in (make-instance 'vector-input :vector input :start start :end (or end (length input)))))
    (uncompress-stream in out)
    out))

(export 'uncompress-vector)

