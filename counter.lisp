(defpackage counter
  (:use #:cl)
  (:nicknames #:m)
  (:export #:make-counter
           #:inc!
           #:dec!
           #:query))

(in-package #:counter)

(defstruct (counter (:constructor make-counter (&key
                                                  (test #'eql)
                                                  (size 100)
                                                  (default-value 0))))
  (table (make-hash-table :test test :size size))
  (default-value default-value))

(defmethod inc! ((counter counter)
                 key
                 value)
  (with-slots (table default-value) counter
    (incf (gethash key table default-value) value)))

(defmethod dec! ((counter counter)
                 key
                 value)
  (with-slots (table default-value) counter
    (decf (gethash key table default-value) value)))

(defmethod query ((counter counter)
                  key)
  (with-slots (table default-value) counter
    (gethash key table default-value)))

#+nil
(let ((m (make-counter)))
  (inc! m 0 1)
  (inc! m 1 3)
  (assert (= 3 (query m 1)))
  (dec! m 1 1)
  (assert (= 2 (query m 1))))

(in-package #:cl-user)
