;;;
;;; BOF
;;;

;; Reference:
;;  「プログラミングコンテストでのデータ構造 2　～平衡二分探索木編～」
;;    https://www.slideshare.net/iwiwi/2-12188757
;;
;; TODO:
;;  - 型をつける
;;  - verify
;;  - range-sum
;;  - range-op (RMQ, RUQ等に対応したい)

(defpackage #:rbst
  (:use #:cl)
  (:nicknames #:rb)
  (:shadow #:merge
           #:remove)
  (:export #:list->rbst
           #:rbst->list
           #:merge
           #:split
           #:insert
           #:remove
           #:insert-preserving-order
           #:remove-preserving-order
           #:ref))

(in-package  #:rbst)

;; xorshift

(deftype uint32 () '(unsigned-byte 32))

(defstruct (xor32 (:constructor build ()))
  (seed #.(1+ (random (1- (ash 1 32)))) :type uint32))

(declaim (ftype (function (xor32) uint32) next))
(defun next (xor32)
  (declare (optimize (speed 3) (safety 0))
           (xor32 xor32))
  (let ((y (xor32-seed xor32)))
    (declare (uint32 y))
    (setf y (logxor y (the uint32 (ash y 13))))
    (setf y (logxor y (the uint32 (ash y -17))))
    (setf y (logxor y (the uint32 (ash y 5))))
    (setf (xor32-seed xor32) y)))

;; rbst

(declaim (xor32 *xorshift*))
(defvar *xorshift* (build))

(deftype uint () '(integer 0 #.most-positive-fixnum))

(deftype maybe (type) `(or null ,type))

(defstruct (rbst (:constructor make-rbst (value &key (left nil) (right nil) (cnt 1) (sum value))))
  (value value :type fixnum)
  (left nil :type (or null rbst))
  (right nil :type (or null rbst))
  (cnt cnt :type uint)
  (sum sum :type uint))

(defun rbst->list (rbst)
  "rbstをlistに変換する。デバッグ用。O(n)"
  (let ((res nil))
    (labels ((%traverse (node)
               ;; 再帰的にpush
               (when node
                 (%traverse (rbst-left node))
                 (push (rbst-value node)
                       res)
                 (%traverse (rbst-right node)))))
      (%traverse rbst)
      (reverse res))))

#+swank
(defmethod print-object ((obj rbst)
                         s)
  (print-unreadable-object (obj s :type t :identity t)
    (princ (rbst->list obj) s)))

(defun list->rbst (list)
  "listをrbstに変換する。デバッグ用。O(n)"
  (let ((xs (copy-seq list)))
    (reduce (lambda (rbst x)
              (merge rbst (make-rbst x :sum x)))
            xs
            :initial-value nil)))

(declaim (inline %get-cnt %get-sum %plus-cnt %plus-sum))
(declaim (ftype (function ((maybe rbst)) uint) %get-cnt))
(defun %get-cnt (rbst)
  (declare ((maybe rbst) rbst))
  (the uint
       (if (null rbst)
           0
           (rbst-cnt rbst))))

(declaim (ftype (function ((maybe rbst)) uint) %get-sum))
(defun %get-sum (rbst)
  (declare ((maybe rbst) rbst))
  (the fixnum
       (if (null rbst)
           0
           (rbst-sum rbst))))

(declaim (ftype (function ((maybe rbst) (maybe rbst)) uint) %plus-cnt))
(defun %plus-cnt (l r)
  (declare ((maybe rbst) l r))
  (the uint
       (+ (%get-cnt l)
          (%get-cnt r))))

(declaim (ftype (function ((maybe rbst) (maybe rbst)) fixnum) %plus-sum))
(defun %plus-sum (l r)
  (declare ((maybe rbst) l r))
  (the fixnum
       (+ (%get-sum l)
          (%get-sum r))))

(declaim (ftype (function ((maybe rbst) (maybe rbst)) (maybe rbst)) merge))
(defun merge (l r)
  "２つのrbstを順序を保ったままマージする。O(logN)"
  (declare ((maybe rbst) l r))
  (when (or (null l)
            (null r))
    (return-from merge (the (maybe rbst)
                            (or l r))))
  (the (maybe rbst)
       (let ((new-cnt (%plus-cnt l r))
             (new-sum (%plus-sum l r)))
         (declare (uint new-cnt)
                  (fixnum new-sum))
         (if (> (rem (next *xorshift*)
                     new-cnt)
                (%get-cnt l))
             ;; lが上
             (make-rbst (rbst-value l)
                         :left (rbst-left l)
                         :right (merge (rbst-right l)
                                       r)
                         :cnt new-cnt
                         :sum new-sum)
             ;; rが上
             (make-rbst (rbst-value r)
                         :left (merge l
                                      (rbst-left r))
                         :right (rbst-right r)
                         :cnt new-cnt
                         :sum new-sum)))))

(declaim (ftype (function ((maybe rbst) uint) (values (maybe rbst) (maybe rbst))) split))
(defun split (rbst key)
  "rbstを分割する。
   返り値: (values left right)

   left:  k未満のnodeからなるrbst
   right: k以上のnodeからなるrbst"
  (declare ((maybe rbst) rbst)
           (uint key))
  (the (values (maybe rbst)
               (maybe rbst))
       (cond
         ((null rbst) (values nil nil))
         ((>= (%get-cnt (rbst-left rbst)) key)
          ;; cntが十分ある => 左
          (multiple-value-bind (new-l new-r)
              (split (rbst-left rbst)
                     key)
            (declare ((maybe rbst) new-l new-r))
            (let* ((r (merge (make-rbst (rbst-value rbst)
                                         :sum (rbst-value rbst)
                                         :cnt 1)
                             (rbst-right rbst)))
                   (res-r (merge new-r r)))
              (declare ((maybe rbst) r res-r))
              (values new-l res-r))))
         (:else
          ;; 右
          (let ((new-key (- key
                            (%get-cnt (rbst-left rbst))
                            1)))
            (declare (uint new-key))
            (multiple-value-bind (new-l new-r)
                (split (rbst-right rbst)
                       new-key)
              (declare ((maybe rbst) new-l new-r))
              (let* ((l (merge (rbst-left rbst)
                               (make-rbst (rbst-value rbst)
                                           :sum (rbst-value rbst)
                                           :cnt 1)))
                     (res-l (merge l new-l)))
                (declare ((maybe rbst) l res-l))
                (values res-l new-r))))))))

#+swank
(define-condition invalid-rbst-index-error (error)
  ((index :reader index :initarg :index)
   (begin :reader begin :initarg :begin)
   (end :reader end :initarg :end))
  (:report (lambda (condition stream)
             (with-slots (index begin end) condition
               (format stream "index must be (integer ~a ~a), not ~a." begin (1- end) index)))))

(defun %check-index (rbst index &key type)
  #-swank (declare (ignore rbst index))
  #+swank
  (let ((end (ecase type
               (:insert (%get-cnt rbst))
               (:remove (1- (%get-cnt rbst))))))
    (unless (<= 0 index end)
      (error 'invalid-rbst-index-error :begin 0
                                        :end end
                                        :index index))))

(declaim (ftype (function ((maybe rbst) uint fixnum) (maybe rbst)) insert))
(defun insert (rbst key value)
  "rbstのkeyの位置にvalueを挿入する。O(logN)"
  (declare ((maybe rbst) rbst)
           (uint key)
           (fixnum value))
  (%check-index rbst key :type :insert)
  (multiple-value-bind (l r)
      (split rbst key)
    (declare ((maybe rbst) l r))
    (the (maybe rbst)
         (merge (merge l (make-rbst value :sum value))
                r))))

(declaim (ftype (function ((maybe rbst) uint) (values (maybe rbst) (maybe rbst))) remove))
(defun remove (rbst key)
  "rbstのkeyの位置にあるvalueを削除する。O(logN)"
  (declare ((maybe rbst) rbst)
           (uint key))
  (%check-index rbst key :type :remove)
  (multiple-value-bind (l c-r)
      (split rbst key)
    (declare ((maybe rbst) l c-r))
    (multiple-value-bind (c r)
        (split c-r 1)
      (declare ((maybe rbst) c r))
      (let ((res (merge l r)))
        (declare ((maybe rbst) res))
        (values res c)))))

(declaim (ftype (function ((maybe rbst) fixnum &optional uint) uint) %find-pos))
(defun %find-pos (rbst value &optional (acc 0))
  (declare ((maybe rbst) rbst)
           (fixnum value)
           (uint acc))
  (if (null rbst)
      acc
      (with-slots (left right) rbst
        (if (<= value (rbst-value rbst))
            (%find-pos left value acc)
            (%find-pos right value (the uint (+ acc (%get-cnt left) 1)))))))

(declaim (ftype (function ((maybe rbst) fixnum) (maybe rbst)) insert-preserving-order remove-preserving-order))
(defun insert-preserving-order (rbst value)
  "valueが昇順ソートされた状態を保ったままvalueを挿入する O(log(n))
   insert, removeと併用すると壊れるため注意"
  (declare ((maybe rbst) rbst)
           (fixnum value))
  (let ((pos (%find-pos rbst value)))
    (declare (uint pos))
    (insert rbst pos value)))

(defun remove-preserving-order (rbst value)
  "valueが昇順ソートされた状態を保ったままvalueを持つkeyを削除する O(log(n))
   insert, removeと併用すると壊れるため注意"
  ;; TODO valueが存在しないときにエラー
  (declare ((maybe rbst) rbst)
           (fixnum value))
  (let ((pos (%find-pos rbst value)))
    (declare (uint pos))
    (remove rbst pos)))

(declaim (ftype (function ((maybe rbst) uint) (maybe fixnum)) ref))
(defun ref (rbst key)
  "rbstのkeyに対応する値を返す。O(logN)"
  ;; TODO
  ;; - 汎変数 setf
  (declare ((maybe rbst) rbst)
           (uint key))
  (multiple-value-bind (_removed center)
      (remove rbst key)
    (declare (ignore _removed)
             ((maybe rbst) center))
    (and center
         (rbst-value center))))

#+swank (load (merge-pathnames "test/rbst.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
