;;;
;;; BOF
;;;

;; xorshift32
;;   reference: https://ja.wikipedia.org/wiki/Xorshift

(defpackage xorshift
  (:use #:cl)
  (:nicknames #:xor)
  (:export #:build
           #:next))

(in-package #:xorshift)

(deftype uint32 () '(unsigned-byte 32))

(defstruct (xor32 (:constructor build ()))
  (seed #.(1+ (random (1- (ash 1 32)))) :type uint32))

(declaim (ftype (function (xor32) uint32) next))
(defun next (xor32)
  (declare (optimize (speed 3) (safety 0))
           (xor32 xor32))
  (let ((y (xor32-seed xor32)))
    (declare (uint32 y))
    (setf y (logxor (the uint32 (ash y 13))))
    (setf y (logxor (the uint32 (ash y -17))))
    (setf y (logxor (the uint32 (ash y 5))))
    (setf (xor32-seed xor32) y)))

;;;
;;; EOF
;;;
