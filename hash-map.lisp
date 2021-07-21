(defpackage hash-map
  (:use #:cl)
  (:nicknames #:m)
  (:export #:make-hash-map
           #:inc!
           #:dec!
           #:query))

(in-package #:hash-map)

(defstruct (hash-map (:constructor make-hash-map (&key
                                                    (test #'eql)
                                                    (size 100))))
  (table (make-hash-table :test test :size size)))

(defmethod inc! ((hash-map hash-map)
                 key
                 value)
  (with-slots (table) hash-map
    (incf (gethash key table 0) value)))

(defmethod dec! ((hash-map hash-map)
                 key
                 value)
  (with-slots (table) hash-map
    (decf (gethash key table 0) value)))

(defmethod query ((hash-map hash-map)
                  key)
  (gethash key (hash-map-table hash-map) 0))

#+nil
(let ((m (%make-hash-map)))
  (inc! m 0 1)
  (inc! m 1 3)
  (assert (= 3 (query m 1)))
  (dec! m 1 1)
  (assert (= 2 (query m 1))))

(in-package #:cl-user)
