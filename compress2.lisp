;;;
;;; BOF
;;;

;; 座標圧縮

(defpackage compress
  (:use #:cl)
  (:nicknames #:comp)
  (:export #:compressed
           #:build
           #:compress
           #:restore))

(in-package #:compress)

(defstruct compressed
  (data nil :type (simple-array integer 1))
  (size nil :type fixnum))

(defmethod build ((sequence sequence))
  (flet ((%remove-duplicates (sequence)
           (coerce (remove-duplicates (coerce sequence 'list))
                   '(simple-array integer 1))))
    (declare (inline %remove-duplicates))
    (let ((uniq (%remove-duplicates sequence)))
      (make-compressed :data (sort uniq #'<)
                       :size (length uniq)))))

(defmethod compress ((compressed compressed) (value integer))
  (with-slots (data size) compressed
    (labels ((%lb (ok ng)
               (declare (fixnum ok ng))
               (if (<= (abs (- ok ng))
                       1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (declare (fixnum mid))
                     (if (< (aref data mid)
                            value)
                         (%lb mid ng)
                         (%lb ok mid))))))
      (1+ (%lb -1 (1+ size))))))

(defmethod restore ((compressed compressed) (value fixnum))
  (with-slots (data size) compressed
    (when (<= 0 value (1- size))
      (aref data value))))

;;;
;;; EOF
;;;
