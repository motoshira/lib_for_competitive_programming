(defpackage suffix-array
  (:use #:cl)
  (:nicknames #:sa))

(in-package #:cl-user)

(deftype uint () '(unsigned-byte 32))

(defstruct (suffix-array (:constructor %make-sa)
                         (:copier nil))
  (data nil :type (simple-array uint (*))))
