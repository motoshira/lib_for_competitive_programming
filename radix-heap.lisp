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

(defstruct pair
  (key 0 :type fixnum)
  (value 0 :type fixnum))

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
    (the (or null pair)
         (gethash (get-pointer pstack idx) table))))

(defun pstack-pop! (pstack idx)
  (with-slots (counter) pstack
    (prog1 (pstack-peak pstack idx)
      ;; remhashしてもいいがタイムロスしそうなので放置
      (decf (aref counter idx)))))

(defun pstack-push! (pstack idx pair)
  (with-slots (table counter) pstack
    (let* ((pointer (get-pointer pstack idx)))
      (setf (gethash pointer table) pair)
      (incf (aref counter idx)))))

(defun clear-pstack (pstacks idx)
  (declare (pseudo-stacks pstacks))
  (with-slots (counter) pstacks
    (setf (aref counter idx) 0)))

(defmacro do-pstack ((pair pstack idx) &body body)
  (let ((cnt (gensym))
        (i (gensym)))
    `(let ((,cnt (get-cnt ,pstack ,idx)))
       (with-slots (table) ,pstack
         (loop for ,i of-type fixnum
               from (encode ,idx 0)
                 below (encode ,idx ,cnt)
               for ,pair of-type pair = (gethash ,i table)
               do ,@body)))))

(defstruct (radix-heap (:constructor make-radix-heap ())
                       (:conc-name heap-))
  (pstacks (make-pseudo-stacks)
   :type pseudo-stacks)
  (size 0 :type uint)
  (last 0 :type uint))

#+swank
(defmethod print-object ((obj radix-heap)
                         stream)
  (print-unreadable-object (obj stream)
    (fresh-line stream)
    (let ((res nil))
      (dotimes (idx *buf-size*)
        (do-pstack (pair (heap-pstacks obj) idx)
          (with-slots (key value) pair
            (push (list key value) res))))
      (dolist (xs (reverse res))
        (princ xs stream)
        (terpri stream)))))

(declaim (inline empty-p get-msb push! pop!))
(defun empty-p (heap)
  (declare (radix-heap heap))
  (zerop (heap-size heap)))

(declaim (inline get-msb))
(defun get-msb (uint)
  (declare (uint uint))
  (let ((ng -1)
        (ok 33))
    (loop while (> (abs (the uint (- ok ng)))
                    1)
          do (let ((mid (ash (the uint (+ ok ng)) -1)))
               (if (>= (the uint (ash 1 mid)) uint)
                   (setf (the uint ok) (the uint mid))
                   (setf (the uint ng) (the uint mid)))))
    (the uint
         (if (= (the uint (ash 1 ok)) uint)
             (1+ ok)
             ok))))

(defun push! (heap key value)
  (declare (radix-heap heap)
           (uint key)
           (t value))
  (with-slots (pstacks size last) heap
    (incf size)
    (let ((idx (get-msb (logxor key last))))
      (declare (uint idx))
      (pstack-push! pstacks
                    idx
                    (make-pair :key key
                               :value value))
      (incf size))))


(defun pop! (heap)
  (declare (radix-heap heap))
  #+swank
  (when (empty-p heap)
    (error "Heap is empty."))
  (with-slots (pstacks size last)
      heap
    (when (pstack-empty-p pstacks 0)
      (let ((idx 1))
        (declare (uint idx))
        (loop while (pstack-empty-p pstacks idx)
              do (incf idx))
        (let ((new-last *inf*))
          (declare (uint new-last))
          (do-pstack (pair pstacks idx)
            (setf new-last
                  (the uint (min new-last
                                 (pair-key pair)))))
          (do-pstack (pair pstacks idx)
            (let ((next (get-msb (logxor (pair-key pair)
                                         new-last))))
              (pstack-push! pstacks next pair)))
          (setf last new-last))))
    (decf size)
    (with-slots (key value)
        (pstack-pop! pstacks 0)
      (values key value))))

#+swank (load (merge-pathnames "test/radix-heap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
