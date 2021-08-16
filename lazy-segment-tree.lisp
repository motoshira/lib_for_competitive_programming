(defpackage lazy-segment-tree
  (:use #:cl)
  (:nicknames #:lseg)
  (:export #:make-lseg))

(in-package #:lseg)

(defstruct (lazy-segment-tree (:conc-name lseg-)
                              (:constructor %make-lseg (size &key data lazy op op-identity update-identity)))
  (data nil :type (simple-array fixnum (*)))
  (lazy nil :type (simple-array fixnum (*)))
  #+nil (acc nil :type (simple-array fixnum (*)))
  (op nil :type function)
  (op-identity nil :type fixnum)
  (update-identity nil :type fixnum)
  (size size :type fixnum))

(defun op-default (x y)
  (declare (fixnum x y))
  (the fixnum (+ x y)))

(defun updater-default (x lazy)
  (declare (ignore x))
  lazy)

(defun modifier-default (x acc lazy) 0)

(defun push-up (lseg idx)
  "値を集約"
  (with-slots (data op) lseg
    (setf (aref data idx)
          (funcall op
                   (aref data (ash idx 1))
                   (aref data (logior 1 (ash idx 1)))))))

(defun push-down (lseg idx)
  "lazyを伝搬"
  (with-slots (data lazy op update-identity) lseg
    (symbol-macrolet ((l (aref lazy (ash idx 1)))
                      (r (aref lazy (logior 1 (ash idx 1)))))
      (setf l (aref lazy idx)
            r (aref lazy idx)
            ;; 自分を更新
            (aref data idx) (aref lazy idx)
            ;; lazyはリセット
            (aref lazy idx) update-identity))))

#+nil
(defun make-lazy-segment-tree (size &key (op #'+) (op-identity 0) (update-identity 0))
  (let* ((m (ash size 1))
         )))
