(defpackage bits-calc
  (:use #:cl)
  (:nicknames #:bits)
  (:export #:get-lsb #:popcount))

(in-package #:bits-calc)

(deftype uint32 () '(unsigned-byte 32))
(deftype uint8 () '(unsigned-byte 8))

(declaim (ftype (function (uint32) uint8) get-lsb))
(defun get-lsb (value)
  (declare (uint32 value))
  (macrolet ((%proc (hex)
               (let ((next? (gensym)))
                 `(let ((,next? (logand (the uint32 value) ,hex)))
                    (declare (uint32 ,next?))
                    (when (plusp ,next?)
                      (setf (the uint32 value) ,next?))))))
    (%proc #xffff0000)
    (%proc #xff00ff00)
    (%proc #xf0f0f0f0)
    (%proc #xcccccccc)
    (%proc #xaaaaaaaa)
    value))

(declaim (inline popcount))
(defun popcount (value)
  (declare (uint32 value))
  (macrolet ((%proc (&rest args)
               `(progn
                  ,@(loop for (hex1 hex2 shift) in args
                          collect `(setf (the uint32 value)
                                         (the uint32
                                              (+ (the uint32
                                                      (logand (the uint32 value)
                                                              ,hex1))
                                                 (the uint32
                                                      (ash (the uint32
                                                                (logand (the uint32 value)
                                                                        ,hex2))
                                                           ,(- shift))))))))))
    (%proc (#x55555555 #xaaaaaaaa 1)
           (#x33333333 #xcccccccc 2)
           (#x0f0f0f0f #xf0f0f0f0 4)
           (#x00ff00ff #xff00ff00 8)
           (#x0000ffff #xffff0000 16))
    (the uint32 value)))
