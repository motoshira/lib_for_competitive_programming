;;;
;;; BOF
;;;

;; deque

(defpackage #:deque
  (:use #:cl)
  (:nicknames #:deq))

(in-package #:deque)

(defparameter *default-deque-size* 300000)

(defstruct (deque (:constructor %make-deque))
  (data nil :type (simple-array t (*)))
  (size nil :type fixnum)
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (count 0 :type fixnum))

(defun make-deque (&optional (size *default-deque-size*))
  (%make-deque
   :size size
   :data (make-array size)))



(defmethod deque-clear ((d deque))
  (setf (deque-size d) 0)
  (setf (deque-head d) 0)
  (setf (deque-tail d) 0)
  (setf (deque-count d) 0))

(declaim (inline empty-p
                 full-p
                 get-prev-index
                 get-next-index))

(defmethod empty-p ((d deque))
  (zerop (deque-count d)))

(defmethod full-p ((d deque))
  (= (deque-count d) (deque-size d)))

(defmethod get-prev-index ((d deque) idx)
  (declare (inline get-next-index))
  (if (zerop idx)
      (1- (deque-size d))
      (1- idx)))

(defmethod get-next-index ((d deque) idx)
  (rem (1+ idx) (deque-size d)))

(defmethod push-front! ((d deque) item)
  (when (full-p d)
    (error "deque is full"))
  (setf (deque-head d) (get-prev-index d (deque-head d)))
  (setf (aref (deque-data d) (deque-head d)) item)
  (when (empty-p d) ; first insersion
    (setf (deque-tail d) (deque-head d)))
  (incf (deque-count d)))

(defmethod push-back! ((d deque) item)
  (when (full-p d)
    (error "deque is full"))
  (setf (deque-tail d) (get-next-index d (deque-tail d)))
  (setf (aref (deque-data d) (deque-tail d)) item)
  (when (empty-p d) ; first insersion
    (setf (deque-head d) (deque-tail d)))
  (incf (deque-count d)))

(defmethod pop-front! ((d deque))
  (when (empty-p d)
    (error "deque is empty,"))
  (let ((value (aref (deque-data d) (deque-head d))))
    (setf (deque-head d) (get-next-index d (deque-head d)))
    (decf (deque-count d))
    value))

(defmethod pop-back! ((d deque))
  (when (empty-p d)
    (error "deque is empty,"))
  (let ((value (aref (deque-data d) (deque-tail d))))
    (setf (deque-tail d) (get-prev-index d (deque-tail d)))
    (decf (deque-count d))
    value))


(defmethod dref ((d deque) index)
  (let ((arr (deque-data d))
        (size (deque-size d))
        (head (deque-head d)))
    (aref arr (mod (+ index head)
                   size))))

;;;
;;; EOF
;;;
