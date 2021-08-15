(defpackage lazy-segment-tree
  (:use #:cl)
  (:nicknames #:lseg)
  (:export #:make-lseg))

(in-package #:lseg)

(defstruct (lazy-segment-tree (:conc-name lseg-)
                              (:constructor %make-lseg (size &key data lazy)))
  (data nil :type (simple-array fixnum (*)))
  (lazy nil :type (simple-array fixnum (*)))
  #+nil (acc nil :type (simple-array fixnum (*)))
  (size size :type fixnum))
