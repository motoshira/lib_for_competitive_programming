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

(defstruct value-container
  (value nil :type fixnum))

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

(defun %heap-to-list (heap comparator)
  (let ((new-heap nil)
        (res nil))
    (loop until (sk:empty-p heap)
          do (multiple-value-bind (key val)
                 (sk:pop! heap comparator)
               (sk:push! new-heap key val comparator)
               (push (list key val)
                     res)))
    (values (reverse res)
            new-heap)))

(defmacro heap->list (heap comparator)
  (let ((res (gensym)))
    (multiple-value-bind (args argvs new setter getter)
        (get-setf-expansion heap)
      `(let ,(mapcar #'list args argvs)
         (multiple-value-bind (,res ,@new)
             (%heap-to-list ,getter ,comparator)
           (declare (list ,res)
                    ((or null heap) ,@new))
           ,setter
           ,res)))))

(defun empty-p (heap)
  (null heap))

(defun peak (heap)
  (declare ((or null heap) heap))
  (if (null heap)
      (values nil nil)
      (values (heap-key heap)
              (unwrap (heap-value heap)))))

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
  (let ((key (gensym))
        (value (gensym)))
    (multiple-value-bind (args argvs val setter getter)
        (get-setf-expansion heap)
      `(let ,(mapcar #'list args argvs)
         (let ((,@val (meld (when ,getter (heap-l ,getter))
                            (when ,getter (heap-r ,getter))
                            ,comparator)))

           (multiple-value-bind (,key ,value)
               (peak ,getter)
             ,setter
             (values ,key ,value)))))))

;;;
;;; EOF
;;;
