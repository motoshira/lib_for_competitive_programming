(defpackage suffix-array
  (:use #:cl)
  (:nicknames #:sa))

(in-package #:cl-user)

(deftype signed-int () '(signed-byte 64))
(deftype uint () '(unsigned-byte 32))

(defstruct (suffix-array (:constructor %make-sa (size))
                         (:copier nil))
  (data (make-array size :element-type 'signed-int
                         :initial-element 0)
   :type (simple-array signed-int (*))))

(deftype rank () '(simple-array signed-int (*)))

(declaim (inline %make-rank))
(defun %make-rank (length)
  (declare (signed-int length))
  (the rank
       (make-array length :initial-element length
                          :element-type 'signed-int)))

(declaim (inline compare))
(defun compare (i j n len rank)
  (declare (uint i j n len)
           (rank rank))
  (or (< (aref rank i)
         (aref rank j))
      (and (= (aref rank i)
              (aref rank j))
           (let ((ri (if (<= (the uint (+ i len)) n)
                         (aref rank (the uint (+ i len)))
                         -1))
                 (rj (if (<= (the uint (+ j len)) n)
                         (aref rank (the uint (+ j len)))
                         -1)))
             (declare (signed-int ri rj))
             (< ri rj)))))

(defun make-suffix-array (string)
  (declare (string string))
  (let* ((string (coerce string 'simple-base-string))
         (n (length string))
         (sa (%make-sa (the signed-int (1+ n))))
         (rank (%make-rank (the signed-int (1+ n))))
         (rank-next (%make-rank (the signed-int (1+ n)))))
    (declare (uint n)
             (simple-base-string string)
             (suffix-array sa)
             (rank rank rank-next))
    (with-slots ((sa-arr data)) sa
      (loop for i of-type uint from 0 to n
            do (setf (aref sa-arr i) i
                     (aref rank i) (if (< i n)
                                       (char-code (char string i))
                                       -1)))
      (let ((len 1))
        (declare (uint len))
        (loop while (<= len n)
              do (setf sa-arr (sort sa-arr (lambda (i j)
                                             (declare (uint i j))
                                             (compare i j n len rank))))
                 (loop for i of-type uint from 1 to n
                       for now of-type signed-int = (aref sa-arr (1- i))
                       for dist of-type signed-int = (aref sa-arr i)
                       for dd of-type bit = (if (compare now dist n len rank)
                                                1
                                                0)
                       do (setf (aref rank-next dist)
                                (the signed-int (+ (aref rank now)
                                                   dd))))
                 (setf rank rank-next))
        sa))))
