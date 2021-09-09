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

(defun heap->list (heap)
  (declare (heap heap))
  (labels ((rec (h acc)
             (if (null h)
                 (reverse acc)
                 (rec (meld (heap-l h)
                            (heap-r h)
                            :comparator #'default-comparator)
                      (cons (list (heap-key h)
                                  (unwrap (heap-value h)))
                            acc)))))
    (rec heap nil)))

(defmethod print-object ((obj heap) s)
  (print-unreadable-object (obj s)
    (princ "HEAP" s)
    (princ (heap->list obj) s)))

(defun empty-p (heap)
  (null heap))

(defun peak (heap)
  (declare ((or null heap) heap))
  (if (null heap)
      (values nil nil)
      (values (heap-key heap)
              (unwrap (heap-value heap)))))

(defun default-comparator (x y)
  (declare (fixnum x y))
  (< x y))

(defun meld (l r &key (comparator #'default-comparator))
  (declare ((or null heap) l r)
           ((function (fixnum fixnum) boolean) comparator))
  (the (or null heap)
       (cond
         ((null l) r)
         ((null r) l)
         ((funcall comparator (heap-key l) (heap-key r))
          (%make-heap (heap-key l)
                      (unwrap (heap-value l))
                      (meld (heap-r l)
                            r
                            :comparator comparator)
                      (heap-l l)))
         (:else
          (%make-heap (heap-key r)
                      (unwrap (heap-value r))
                      (heap-r r)
                      (meld l
                            (heap-l r)
                            :comparator comparator))))))

(defmacro push! (heap key value &key (comparator 'default-comparator))
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion heap)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (meld (%make-heap ,key ,value nil nil) ,getter :comparator #',comparator)))
         ,setter))))

(defmacro pop! (heap &key (comparator 'default-comparator))
  (let ((key (gensym))
        (value (gensym)))
    (multiple-value-bind (args argvs val setter getter)
        (get-setf-expansion heap)
      `(let ,(mapcar #'list args argvs)
         (let ((,@val (meld (when ,getter (heap-l ,getter))
                            (when ,getter (heap-r ,getter))
                            :comparator #',comparator)))

           (multiple-value-bind (,key ,value)
               (peak ,getter)
             ,setter
             (values ,key ,value)))))))

;;;
;;; EOF
;;;
