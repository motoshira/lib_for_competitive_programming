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

(declaim (inline pstack-empty-p))
(defun pstack-empty-p (pstack)
  (declare ((or null pair-stack) pstack))
  (null pstack))

(defmacro do-pstack (((l r) pstack) &body body)
  (let ((ps (gensym)))
    `(let ((,ps ,pstack))
       (declare ((or null pair-stack) ,ps))
       (loop until (pstack-empty-p ,ps)
             do (multiple-value-bind (,l ,r)
                    (pstack-pop! ,ps)
                  (declare (uint ,l ,r))
                  ,@body)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *buf-size* 33)
  (defparameter *inf* (1- (ash 1 32))))

(defstruct (radix-heap (:constructor make-radix-heap ())
                       (:conc-name heap-))
  (buf (make-array #.*buf-size* :element-type '(or null pair-stack)
                                :initial-element nil)
   :type (simple-array (or null pair-stack) (#.*buf-size*)))
  (size 0 :type uint)
  (last 0 :type uint))

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
           (uint key value))
  (with-slots (buf size last) heap
    (incf size)
    (let ((pos (get-bit (logxor key last))))
      (declare (uint pos))
      (pstack-push! (key value) (aref buf pos)))))


(defun pop! (heap)
  (declare (radix-heap heap))
  #+swank
  (when (empty-p heap)
    (error "Heap is empty."))
  (with-slots (buf size last)
      heap
    (when (pstack-empty-p (aref buf 0))
      (let ((idx 1))
        (declare (uint idx))
        (loop while (pstack-empty-p (aref buf idx))
              do (incf idx))
        (let ((new-last #.*inf*))
          (declare (uint new-last))
          (do-pstack ((key _value) (aref buf idx))
            (declare (ignore _value)
                     (uint key))
            (setf new-last (min new-last key)))
          (do-pstack ((key value) (aref buf idx))
            (let ((next (get-bit (logxor key new-last))))
              (declare (uint next))
              (pstack-push! (key value) (aref buf next))))
          (setf (aref buf idx) nil))))
    (decf size)
    (pstack-pop! (aref buf 0))))

#+swank (load (merge-pathnames "test/radix-heap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
