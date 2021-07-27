(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st)
  (:export #:build
           #:fold
           #:update
           #:range-update
           #:dump))

(in-package #:st)

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil :type (simple-array fixnum (*)))
  (op-acc nil :type (simple-array fixnum (*)))
  (update-lazy nil :type (simple-array fixnum (*)))
  (k 0 :type fixnum)
  (op nil :type function)
  (op-lazy nil :type function)
  (e 0 :type fixnum))

(defun dump (st)
  (with-slots (e) st
    (let ((res nil))
      (dotimes (i (%size st))
        (let ((val (fold st i (1+ i))))
          (when (/= e val)
            (push (list i val)
                  res))))
      (sort res #'< :key #'first))))

#+nil
(let ((xs (list 1 4 6))
      (ys (list 3 1 5))
      (st (build 7)))
  (dolist (x xs)
    (update st x 1))
  (assert (equal (dump st)
                 '((1 1)
                   (4 1)
                   (6 1))))
  (dolist (y ys)
    (update st y 2))
  (assert (equal (dump st)
                 '((1 2)
                   (3 2)
                   (4 1)
                   (5 2)
                   (6 1)))))

(defun build (size &key (op #'+) (op-lazy #'*) (e 0))
  (let* ((k (isqrt size))
         (sub-size (ceiling size k)))
    (flet ((%make-arr (size)
             (make-array size :element-type 'fixnum
                              :initial-element e)))
      (%make-st :main (%make-arr size)
                :op-acc (%make-arr sub-size)
                :update-lazy (%make-arr sub-size)
                :k k
                :op op
                :op-lazy op-lazy
                :e e))))

(defun %size (st)
  (length (st-main st)))

#+nil
(let ((st (st:build 22)))
  (assert (= (%size st)
             22)))

(defun %sub-idx (st idx)
  (with-slots (k) st
    (floor idx k)))

#+nil
(let ((st (st:build 20)))
  (assert (= (st::st-k st) 4))
  (assert (= (st::%sub-idx st 5)
             1))
  (assert (= (st::%sub-idx st 10)
             2)))

(defun %%propagate! (st i)
  (with-slots (main update-lazy) st
    (setf (aref main i)
          (aref update-lazy (%sub-idx st i)))))

(defun lazy-p (st sub-idx)
  (with-slots (update-lazy e) st
    (/= e (aref update-lazy sub-idx))))

(defun %propagate! (st idx)
  (with-slots (update-lazy k e) st
    (let* ((sub-idx (%sub-idx st idx))
           (size (%size st))
           (idx-begin (min (1- size) (* sub-idx k)))
           (idx-end (min size (+ idx-begin k))))
      ;; 初期値でなければ伝搬する
      (when (lazy-p st sub-idx)
        (loop for i from idx-begin below idx-end
              do (%%propagate! st i))
        (setf (aref update-lazy sub-idx)
              e)))))

(defun %propagate-lazy (st idx)
  (with-slots (update-lazy k e) st
    (let* ((sub-idx (%sub-idx st idx)))
      (%propagate! st idx))))

(defun %update-main! (st idx value)
  (with-slots (main) st
    (setf (aref main idx) value)))

(defun %propagate-op-acc! (st idx)
  ;; idxの該当するブロックに対応するop-accを更新
  (with-slots (main op-acc k e op) st
    (let* ((sub-idx (%sub-idx st idx))
           (size (%size st))
           (idx-begin (min (1- size) (* k sub-idx)))
           (idx-end (min size (+ idx-begin k)))
           (tmp e))
      (loop for i from idx-begin below idx-end
            ;; 伝搬済みのはずなのでmainをみればOK
            for value = (aref main i)
            do (setf tmp
                     (funcall op tmp i)))
      (setf (aref op-acc sub-idx) tmp))))

(defun %update-lazy! (st idx value)
  (with-slots (update-lazy k) st
    (let ((sub-idx (%sub-idx st idx)))
      (setf (aref update-lazy sub-idx)
            value))))

(defun %update-op-acc! (st idx value)
  (with-slots (op-acc op-lazy k) st
    (let* ((sub-idx (%sub-idx st idx))
           (acc-value (funcall op-lazy value k)))
      (setf (aref op-acc sub-idx) acc-value))))

(defmacro while (test &body body)
  `(loop while ,test
         do ,@body))

(defun range-update (st l r value)
  "[l,r)をvalueで更新する"
  (with-slots (k) st
    (let ((begin l)
          (end r)
          (ll (* k (ceiling l k)))
          (rr (* k (floor r k))))
      (%propagate-lazy st begin)
      (%propagate-lazy st end)
      (while (and (< l ll)
                  (< l r))
        (%update-main! st l value)
        (incf l))
      (%propagate-op-acc! st begin)
      (while (and (< rr r)
                  (< l r))
        (decf r)
        (%update-main! st r value))
      (%propagate-op-acc! st end)
      (while (< l r)
        (%update-lazy! st l value)
        (%update-op-acc! st l value)
        (incf l k)))))

(defun update (st idx value)
  (range-update st idx (1+ idx) value))

(defun fold (st l r)
  (with-slots (main k e op op-acc) st
    (let ((begin l)
          (end r)
          (ll (* k (ceiling l k)))
          (rr (* k (floor r k)))
          (res e))
      (flet ((%update-res! (value)
               (setf res
                     (funcall op res value))))
        (%propagate-lazy st begin)
        (%propagate-lazy st end)
        (while (and (< l ll)
                    (< l r))
          (%update-res! (aref main l))
          (incf l))
        (while (and (< rr r)
                    (< l r))
          (decf r)
          (%update-res! (aref main r)))
        (while (< l r)
          (%update-res! (aref op-acc (%sub-idx st l)))
          (incf l k))
        res))))

(in-package #:cl-user)
