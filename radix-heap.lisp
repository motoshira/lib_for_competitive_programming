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

(defstruct (pair-stack (:constructor make-pstack)
                       (:conc-name ps-))
  (l 0 :type uint)
  (r 0 :type uint)
  (tail nil :type (or null pair-stack)))

(defmacro pstack-push! ((l r) pstack)
  (multiple-value-bind (args argvs new setter accessor)
      (get-setf-expansion pstack)
    `(let ,(mapcar #'list args argvs)
       (let ((,@new (make-pstack :l ,l
                                 :r ,r
                                 :tail ,accessor)))
         ,setter))))

(defmacro pstack-pop! (pstack)
  (let ((l (gensym))
        (r (gensym))
        (tail (gensym)))
    (multiple-value-bind (args argvs new setter accessor)
        (get-setf-expansion pstack)
      `(let ,(mapcar #'list args argvs)
         (with-slots ((,l l)
                      (,r r)
                      (,tail tail))
             ,accessor
           (let ((,@new ,tail))
             ,setter
             (values ,l ,r)))))))

(defun pstack-empty-p (pstack)
  (null pstack))

(defmacro do-pstack (((l r) pstack) &body body)
  (let ((ps (gensym)))
    `(let ((,ps ,pstack))
       (loop until (pstack-empty-p ,ps)
             do (multiple-value-bind (,l ,r)
                    (pstack-pop! ,ps)
                  ,@body)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *buf-size* 33))

(defstruct (radix-heap (:conc-name heap-))
  (buf (make-array #.*buf-size* :element-type '(or null pair-stack)
                                :initial-element nil)
   :type (simple-array (or null pair-stack) (#.*buf-size*)))
  (size 0 :type uint)
  (last 0 :type uint))

(defun empty-p (heap)
  (zerop (heap-size heap)))

(defun get-bit (uint)
  (let ((cnt 0))
    (loop while (plusp uint)
          do (incf cnt)
             (setf uint (ash uint -1)))
    cnt))

(defun push! (heap key value)
  (with-slots (buf size last) heap
    (incf size)
    (let ((pos (logxor key last)))
      (pstack-push! (key value) (aref buf pos)))))

#+swank (load (merge-pathnames "test/radix-heap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)
