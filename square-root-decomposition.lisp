(defpackage squqre-root-decomposition
  (:use #:cl)
  (:nicknames #:sd)
  (:shadow #:+ #:- #:* #:floor #:min #:max #:<=)
  (:export #:make-square-root-decomposition
           #:update!
           #:fold))

(in-package #:sd)

(macrolet ((%def (op cl-op result-type)
             `(progn
                (declaim (inline ,op))
                (defun ,op (x y)
                  (declare (fixnum x y)
                           (ignorable x y))
                  (the ,result-type (,cl-op x y))))))
  (%def + cl:+ fixnum)
  (%def - cl:- fixnum)
  (%def * cl:* fixnum)
  (%def min cl:min fixnum)
  (%def max cl:max fixnum)
  (%def floor cl:floor fixnum)
  (%def <= cl:<= boolean))

(defstruct functions
  (op nil :type (function (fixnum fixnum) fixnum))
  (updater nil :type (function (fixnum fixnum) fixnum))
  (op-identity nil :type fixnum))

(defstruct (bucket (:constructor %make-bucket))
  (data nil)
  (lazy nil :type (or null fixnum))
  (acc nil :type (or null ))
  (l nil)
  (r nil)
  (size nil :type fixnum)
  (op nil :type (function (fixnum fixnum) fixnum))
  (op-identity nil :type fixnum)
  (updater nil :type (function (fixnum fixnum) fixnum)))

(defstruct (square-root-decomp (:constructor %make-square-root-decomp)
                               (:conc-name sd-))
  (buckets nil :type (simple-array bucket 1))
  (op nil :type (function (fixnum fixnum) fixnum))
  (total-size nil :type fixnum)
  (bucket0-size nil :type fixnum))

(defun %update-all! (bucket val)
  (setf (bucket-lazy bucket) val))

(defun %reduce-bucket (bucket)
  (let ((op (bucket-op bucket)))
    (reduce (lambda (acc x)
              (funcall op acc x))
            (bucket-data bucket)
            :initial-value (bucket-op-identity bucket))))

(defun %push-down! (bucket)
  "Propagate lazy"
  (when (bucket-lazy bucket)
    (loop for i below (bucket-size bucket)
          do (setf (aref (bucket-data bucket) i)
                   (funcall (bucket-updater bucket)
                            (aref (bucket-data bucket) i)
                            (bucket-lazy bucket))))
    (setf (bucket-lazy bucket) nil)))

(defun %push-up! (bucket)
  "Update acc"
  (assert (null (bucket-lazy bucket)))
  (setf (bucket-acc bucket) (%reduce-bucket bucket)))

(defun %update-partially! (bucket l r val)
  (%push-down! bucket)
  (let ((ll (bucket-l bucket)))
    (loop for idx
          from (max l ll)
            below (min r (bucket-r bucket))
          do (setf (aref (bucket-data bucket)
                         (- idx ll))
                   (funcall (bucket-updater bucket)
                            (aref (bucket-data bucket) (- idx ll))
                            val))))
  (%push-up! bucket))

(defun %update! (bucket l r val)
  (unless (or (<= r (bucket-l bucket))
              (<= (bucket-r bucket) l))
    (cond
      ((and (<= (bucket-l bucket) l)
            (<= r (bucket-r bucket)))
       (%update-all! bucket val))
      (:else
       (%update-partially! bucket l r val)))))

(defun update! (sd l r val)
  (loop for bucket across (sd-buckets sd)
        do (%update! bucket l r val)))


(defun fold (sd l r)
  (reduce (lambda (acc bucket)
            (funcall (sd-op sd)
                     acc
                     (%reduce-bucket bucket)))
          :initial-value (bucket-op-identity bucket)))
