;;;
;;; BOF
;;;

;; Skew Heap
;; Reference: http://hos.ac/blog/#blog0001

(defpackage skew-heap
  (:use #:cl)
  (:nicknames #:sk)
  (:export #:heap
           #:heap->list
           #:empty-p
           #:meld
           #:peak
           #:push!
           #:pop!))

(in-package #:sk)

(defstruct (heap (:constructor make-heap (key value &key l r)))
  (key key :type fixnum)
  (value value :type t)
  (l nil :type (or null heap))
  (r nil :type (or null heap)))

(defun heap->list (heap)
  (let ((res nil))
    (labels ((%traverse (node)
               (when node
                 (with-slots (key value l r)
                     node
                   (push (list key value) res)
                   (%traverse l)
                   (%traverse r)))))
      (%traverse heap)
      (reverse res))))

(defun empty-p (heap)
  (null heap))

(defun peak (heap)
  (if (null heap)
      (values nil nil)
      (values (heap-key heap)
              (heap-value heap))))

(defun meld (l r)
  (declare ((or null heap) l r))
  (the (or null heap)
       (cond
         ((null l) r)
         ((null r) l)
         (:else (when (> (heap-key l) (heap-key r))
                  (rotatef l r))
                (setf (heap-r l) (meld (heap-r l) r))
                (rotatef (heap-l l) (heap-r l))
                l))))

(defmacro push! (heap key value)
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion heap)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (meld (make-heap ,key ,value) ,getter)))
         ,setter))))

(defmacro pop! (heap)
  (let ((key (gensym))
        (value (gensym)))
    (multiple-value-bind (args argvs val setter getter)
        (get-setf-expansion heap)
      `(let ,(mapcar #'list args argvs)
         (let ((,@val (meld (when ,getter (heap-l ,getter))
                            (when ,getter (heap-r ,getter)))))
           (multiple-value-bind (,key ,value)
               (peak ,getter)
             ,setter
             (values ,key ,value)))))))

;;;
;;; EOF
;;;
