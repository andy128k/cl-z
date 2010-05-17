;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage cl-z-system
  (:use :common-lisp :asdf))

(in-package :cl-z-system)

(defsystem "cl-z"
  :description "ZLib wrapper"
  :version "0.10.2.2"
  :author "Andrey Kutejko <andy128k@gmail.com>"
  :licence "LGPL"
  :depends-on (:cffi)
  :components ((:file "cl-z")))

