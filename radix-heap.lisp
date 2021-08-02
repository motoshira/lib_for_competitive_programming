(defpackage radix-heap
  (:use #:cl)
  (:nicknames #:rd))

(in-package #:rd)

(deftype uint () '(unsigned-byte 32))

(defstruct pair
  (pri 0 :type uint)
  (pos 0 :type uint))

(defstruct radix-heap
  ())
