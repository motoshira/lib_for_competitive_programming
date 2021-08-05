;;;
;;; BOF
;;;

;; Radix Heap
;; reference:
;;   https://ei1333.github.io/algorithm/radix-heap.html

(defpackage radix-heap
  (:use #:cl)
  (:nicknames #:rd)
  (:export #:make-radix-heap
           #:heap-size
           #:empty-p
           #:push!
           #:pop!))

(in-package #:rd)

(deftype uint () '(unsigned-byte 32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *buf-size* 33)
  (defparameter *inf* (1- (ash 1 32)))
  (defparameter *max-stack-size* 1000000)
  (defparameter *initial-stack-size* 1000))

(defstruct pseudo-stacks
  (table (make-hash-table :test #'eq) :type hash-table)
  (counter (make-array *buf-size* :element-type 'fixnum
                                  :initial-element 0)
   :type (simple-array fixnum (#.*buf-size*))))

(defun encode (idx cnt)
  (+ (* idx *max-stack-size*)
     cnt))

(defun get-cnt (pstack idx)
  (with-slots (counter) pstack
    (aref counter idx)))

(defun pstack-empty-p (pstack idx)
  (zerop (get-cnt pstack idx)))

(defun get-pointer (pstack idx)
  (let ((cnt (get-cnt pstack idx)))
    (encode idx cnt)))

(defun pstack-peak (pstack idx)
  (with-slots (table) pstack
    (gethash (get-pointer pstack idx) table)))

(defun pstack-pop! (pstack idx)
  (with-slots (counter) pstack
    (prog1 (pstack-peak pstack idx)
      ;; remhashしてもいいがタイムロスしそうなので放置
      (decf (aref counter idx)))))

(defun pstack-push! (pstack idx value)
  (with-slots (table counter) pstack
    (let* ((pointer (get-pointer pstack idx)))
      (setf (gethash pointer table) value)
      (incf (aref counter idx)))))

(defmacro do-pstack ((x pstack idx) &body body)
  (let ((cnt (gensym))
        (table (gensym))
        (i (gensym)))
    `(let ((,cnt (get-cnt ,pstack ,idx)))
       (with-slots (,table) ,pstack
         (loop for ,i of-type fixnum
               from (encode ,idx 0)
                 below (encode ,idx ,cnt)
               for ,x = (gethash ,i ,table)
               do ,@body)))))

(defstruct (radix-heap (:constructor make-radix-heap ())
                       (:conc-name heap-))
  (keys (make-pseudo-stacks)
   :type pseudo-stacks)
  (values (make-pseudo-stacks)
   :type pseudo-stacks)
  (size 0 :type uint)
  (last 0 :type uint))

#+swank
(defmethod print-object ((obj radix-heap)
                         stream)
  (print-unreadable-object (obj stream)
    (fresh-line stream)
    (loop for xs in (sort (loop for kv across (heap-keys obj)
                                for vv across (heap-values obj)
                                append (loop for k across kv
                                             for v across vv
                                             collect (cons k v)))
                          #'<
                          :key #'first)
          do (princ xs stream)
             (terpri stream))))

(declaim (inline empty-p get-bit push! pop!))
(defun empty-p (heap)
  (declare (radix-heap heap))
  (zerop (heap-size heap)))

(defun get-bit (uint)
  (declare (uint uint))
  (let ((cnt 0))
    (declare (uint cnt))
    (loop while (plusp uint)
          do (incf cnt)
             (setf uint (ash uint -1)))
    cnt))

(defun push! (heap key value)
  (declare (radix-heap heap)
           (uint key)
           (t value))
  (with-slots (keys values size last) heap
    (incf size)
    (let ((pos (get-bit (logxor key last))))
      (declare (uint pos))
      (vector-push-extend key (aref keys pos))
      (vector-push-extend value (aref values pos))
      heap)))


(defun pop! (heap)
  (declare (radix-heap heap))
  #+swank
  (when (empty-p heap)
    (error "Heap is empty."))
  (with-slots (keys values size last)
      heap
    (when (zerop (fill-pointer (aref keys 0)))
      (let ((idx 1))
        (declare (uint idx))
        (loop while (zerop (fill-pointer (aref keys idx)))
              do (incf idx))
        (let ((new-last (reduce #'min (aref keys idx))))
          (declare (uint new-last))
          (loop repeat (fill-pointer (aref keys idx))
                for key of-type uint = (vector-pop (aref keys idx))
                for value of-type uint = (vector-pop (aref values idx))
                for next of-type uint = (get-bit (logxor key new-last))
                do (vector-push-extend key (aref keys next))
                   (vector-push-extend value (aref values next)))
          (setf last new-last))))
    (decf size)
    (values (vector-pop (aref keys 0))
            (vector-pop (aref values 0)))))

#+swank (load (merge-pathnames "test/radix-heap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
