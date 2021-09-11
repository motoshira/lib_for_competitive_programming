;;;
;;; BOF
;;;

;; Skew Heap
;; Reference: http://hos.ac/blog/#blog0001

(defpackage skew-heap
  (:use #:cl)
  (:nicknames #:sk)
  (:export #:heap
           #:%make-heap
           #:heap->list
           #:empty-p
           #:meld
           #:peak
           #:push!
           #:pop!))

(in-package #:sk)

;; Change here according to the problem

(defstruct value-container
  (value nil :type list))

(declaim (inline unwrap))
(defun unwrap (value-container)
  (declare (value-container value-container))
  (value-container-value value-container))

(defstruct (heap (:constructor %make-heap (key val l r)))
  (key key :type fixnum)
  (value (make-value-container :value val) :type value-container)
  (l nil :type (or null heap))
  (r nil :type (or null heap)))

;; TODO Redefine with setf macro

(defun empty-p (heap)
  (null heap))

(defun peak (heap)
  (declare ((or null heap) heap))
  (if (null heap)
      nil
      (unwrap (heap-value heap))))

(defun meld (l r comparator)
  (declare ((or null heap) l r)
           ((function (fixnum fixnum) boolean) comparator))
  (the (or null heap)
       (cond
         ((null l) r)
         ((null r) l)
         (:else
          (unless (funcall comparator (heap-key l) (heap-key r))
            (rotatef l r))
          (setf (heap-r l)
                (meld (heap-r l) r comparator))
          (rotatef (heap-l l)
                   (heap-r l))
          l))))

(defmacro push! (heap key value comparator)
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion heap)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (meld (%make-heap ,key ,value nil nil) ,getter ,comparator)))
         ,setter))))

(defmacro pop! (heap comparator)
  (let ((value (gensym)))
    (multiple-value-bind (args argvs val setter getter)
        (get-setf-expansion heap)
      `(let ,(mapcar #'list args argvs)
         (let ((,@val (meld (when ,getter (heap-l ,getter))
                            (when ,getter (heap-r ,getter))
                            ,comparator)))

           (let ((,value (peak ,getter)))
             ,setter
             ,value))))))

;;;
;;; EOF
;;;
