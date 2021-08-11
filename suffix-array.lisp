(defpackage suffix-array
  (:use #:cl)
  (:nicknames #:sa)
  (:shadow #:find)
  (:export #:make-suffix-array
           #:find))

(in-package #:suffix-array)

(deftype %signed-int () '(signed-byte 64))
(deftype %unsigned-int () '(unsigned-byte 32))

(defstruct (suffix-array (:constructor %make-sa (size))
                         (:copier nil))
  (data (make-array size :element-type '%signed-int
                         :initial-element 0)
   :type (simple-array %signed-int (*))))

(deftype rank () '(simple-array %signed-int (*)))

(declaim (inline %make-rank))
(defun %make-rank (length)
  (declare (%signed-int length))
  (the rank
       (make-array length :initial-element length
                          :element-type '%signed-int)))

(declaim (inline compare))
(defun compare (i j n len rank)
  (declare (%unsigned-int i j n len)
           (rank rank))
  (or (< (aref rank i)
         (aref rank j))
      (and (= (aref rank i)
              (aref rank j))
           (let ((ri (if (<= (the %unsigned-int (+ i len)) n)
                         (aref rank (the %unsigned-int (+ i len)))
                         -1))
                 (rj (if (<= (the %unsigned-int (+ j len)) n)
                         (aref rank (the %unsigned-int (+ j len)))
                         -1)))
             (declare (%signed-int ri rj))
             (< ri rj)))))

(defun make-suffix-array (string)
  (declare (string string))
  (let* ((string (coerce string 'simple-base-string))
         (n (length string))
         (sa (%make-sa (the %unsigned-int (1+ n))))
         (rank (%make-rank (the %unsigned-int (1+ n))))
         (rank-next (%make-rank (the %unsigned-int (1+ n)))))
    (declare (%unsigned-int n)
             (simple-base-string string)
             (suffix-array sa)
             (rank rank rank-next))
    (with-slots ((sa-arr data)) sa
      (loop for i of-type %unsigned-int from 0 to n
            do (setf (aref sa-arr i) i
                     (aref rank i) (if (< i n)
                                       (char-code (char string i))
                                       -1)))
      (let ((len 1))
        (declare (%unsigned-int len))
        (loop while (<= len n)
              do (setf sa-arr (sort sa-arr (lambda (i j)
                                             (declare (%unsigned-int i j))
                                             (compare i j n len rank))))
                 (loop for i of-type %unsigned-int from 1 to n
                       for now of-type %signed-int = (aref sa-arr (1- i))
                       for dist of-type %signed-int = (aref sa-arr i)
                       for dd of-type bit = (if (compare now dist n len rank)
                                                1
                                                0)
                       do (setf (aref rank-next dist)
                                (the %signed-int (+ (aref rank-next now)
                                                   dd))))
                 (setf rank rank-next)
                 (setf len (ash len 1)))
        sa))))

(defun find (main-string sub-string &optional (main-sa (make-suffix-array main-string)))
  "Returns T is main-string contains sub-string as substring."
  (declare (string main-string sub-string)
           (suffix-array main-sa))
  (with-slots ((sa-arr data)) main-sa
    (let ((ok 0)
          (ng (length main-string))
          (m (length sub-string))
          (main-string (coerce main-string 'simple-base-string))
          (sub-string (coerce sub-string 'simple-base-string)))
      (declare (%unsigned-int ok ng)
               (simple-base-string main-string sub-string))
      (loop while (> (abs (- ok ng)) 1)
            for mid of-type %unsigned-int = (ash (+ ok ng) -1)
            for start of-type %signed-int = (aref sa-arr mid)
            do (cond
                 ((string< main-string sub-string :start1 start
                                                  :end1 (the %unsigned-int (+ start m)))
                  (setf (the %unsigned-int ok) mid))
                 (:else
                  (setf (the %unsigned-int ng) mid))))
      (let ((start (aref sa-arr (the %unsigned-int (1+ ok)))))
        (declare (%unsigned-int start))
        (string= main-string sub-string :start1 start
                                        :end1 (the %unsigned-int (+ start m)))))))
