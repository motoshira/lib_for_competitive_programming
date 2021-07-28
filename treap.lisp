;;;
;;; BOF
;;;

;; Immutable Treap
;; Reference:
;;  「プログラミングコンテストでのデータ構造 2　～平衡二分探索木編～」
;;    https://www.slideshare.net/iwiwi/2-12188757
;;
;; TODO:
;;  - 型をつける
;;  - verify
;;  - range-sum
;;  - range-op (RMQ, RUQ等に対応したい)

(defpackage #:treap
  (:use #:cl)
  (:nicknames #:tr)
  (:shadow #:merge
           #:remove)
  (:export #:list->treap
           #:treap->list
           #:merge
           #:split
           #:insert
           #:remove
           #:insert-preserving-order
           #:remove-preserving-order
           #:ref))

(in-package  #:treap)

(deftype uint () '(integer 0 #.most-positive-fixnum))

(deftype maybe (type) `(or null ,type))

(defstruct (treap (:constructor make-treap (value &key (left nil) (right nil) (cnt 1) (sum value))))
  (value value :type fixnum)
  (left nil :type (or null treap))
  (right nil :type (or null treap))
  (priority (random #.most-positive-fixnum) :type uint)  ;; 勝手に決まる
  (cnt cnt :type uint)
  (sum sum :type uint))

(defun treap->list (treap)
  "treapをlistに変換する。デバッグ用。O(n)"
  (let ((res nil))
    (labels ((%traverse (node)
               ;; 再帰的にpush
               (when node
                 (%traverse (treap-left node))
                 (push (treap-value node)
                       res)
                 (%traverse (treap-right node)))))
      (%traverse treap)
      (reverse res))))

#+swank
(defmethod print-object ((obj treap)
                         s)
  (print-unreadable-object (obj s :type t :identity t)
    (princ (treap->list obj) s)))

(defun list->treap (list)
  "listをtreapに変換する。デバッグ用。O(n)"
  (let ((xs (copy-seq list)))
    (reduce (lambda (treap x)
              (merge treap (make-treap x :sum x)))
            xs
            :initial-value nil)))

(declaim (inline %get-cnt %get-sum %plus-cnt %plus-sum))
(declaim (ftype (function ((maybe treap)) uint) %get-cnt))
(defun %get-cnt (treap)
  (declare ((maybe treap) treap))
  (the uint
       (if (null treap)
           0
           (treap-cnt treap))))

(declaim (ftype (function ((maybe treap)) uint) %get-sum))
(defun %get-sum (treap)
  (declare ((maybe treap) treap))
  (the fixnum
       (if (null treap)
           0
           (treap-sum treap))))

(declaim (ftype (function ((maybe treap) (maybe treap)) uint) %plus-cnt))
(defun %plus-cnt (l r)
  (declare ((maybe treap) l r))
  (the uint
       (+ (%get-cnt l)
          (%get-cnt r))))

(declaim (ftype (function ((maybe treap) (maybe treap)) fixnum) %plus-sum))
(defun %plus-sum (l r)
  (declare ((maybe treap) l r))
  (the fixnum
       (+ (%get-sum l)
          (%get-sum r))))

(declaim (ftype (function ((maybe treap) (maybe treap)) (maybe treap)) merge))
(defun merge (l r)
  "２つのtreapを順序を保ったままマージする。O(logN)"
  (declare ((maybe treap) l r))
  (when (or (null l)
            (null r))
    (return-from merge (the (maybe treap)
                            (or l r))))
  (the (maybe treap)
       (let ((new-cnt (%plus-cnt l r))
             (new-sum (%plus-sum l r)))
         (declare (uint new-cnt)
                  (fixnum new-sum))
         (if (> (treap-priority l)
                (treap-priority r))
             ;; lが上
             (make-treap (treap-value l)
                         :left (treap-left l)
                         :right (merge (treap-right l)
                                       r)
                         :cnt new-cnt
                         :sum new-sum)
             ;; rが上
             (make-treap (treap-value r)
                         :left (merge l
                                      (treap-left r))
                         :right (treap-right r)
                         :cnt new-cnt
                         :sum new-sum)))))

(declaim (ftype (function ((maybe treap) uint) (values (maybe treap) (maybe treap))) split))
(defun split (treap key)
  "treapを分割する。
   返り値: (values left right)

   left:  k未満のnodeからなるtreap
   right: k以上のnodeからなるtreap"
  (declare ((maybe treap) treap)
           (uint key))
  (the (values (maybe treap)
               (maybe treap))
       (cond
         ((null treap) (values nil nil))
         ((>= (%get-cnt (treap-left treap)) key)
          ;; cntが十分ある => 左
          (multiple-value-bind (new-l new-r)
              (split (treap-left treap)
                     key)
            (declare ((maybe treap) new-l new-r))
            (let* ((r (merge (make-treap (treap-value treap)
                                         :sum (treap-value treap)
                                         :cnt 1)
                             (treap-right treap)))
                   (res-r (merge new-r r)))
              (declare ((maybe treap) r res-r))
              (values new-l res-r))))
         (:else
          ;; 右
          (let ((new-key (- key
                            (%get-cnt (treap-left treap))
                            1)))
            (declare (uint new-key))
            (multiple-value-bind (new-l new-r)
                (split (treap-right treap)
                       new-key)
              (declare ((maybe treap) new-l new-r))
              (let* ((l (merge (treap-left treap)
                               (make-treap (treap-value treap)
                                           :sum (treap-value treap)
                                           :cnt 1)))
                     (res-l (merge l new-l)))
                (declare ((maybe treap) l res-l))
                (values res-l new-r))))))))

#+swank
(define-condition invalid-treap-index-error (error)
  ((index :reader index :initarg :index)
   (begin :reader begin :initarg :begin)
   (end :reader end :initarg :end))
  (:report (lambda (condition stream)
             (with-slots (index begin end) condition
               (format stream "index must be (integer ~a ~a), not ~a." begin (1- end) index)))))

(defun %check-index (treap index &key type)
  #-swank (declare (ignore treap index))
  #+swank
  (let ((end (ecase type
               (:insert (%get-cnt treap))
               (:remove (1- (%get-cnt treap))))))
    (unless (<= 0 index end)
      (error 'invalid-treap-index-error :begin 0
                                        :end end
                                        :index index))))

(defun insert (treap key value)
  "treapのkeyの位置にvalueを挿入する。O(logN)"
  (%check-index treap key :type :insert)
  (multiple-value-bind (l r)
      (split treap key)
    (merge (merge l (make-treap value :sum value))
           r)))

(defun remove (treap key)
  "treapのkeyの位置にあるvalueを削除する。O(logN)"
  (%check-index treap key :type :remove)
  (multiple-value-bind (l c-r)
      (split treap key)
    (multiple-value-bind (c r)
        (split c-r 1)
      (let ((res (merge l r)))
        (values res c)))))

(defun %find-pos (treap value &optional (acc 0))
  (if (null treap)
      acc
      (with-slots (left right) treap
        (if (<= value (treap-value treap))
            (%find-pos left value acc)
            (%find-pos right value (+ acc (%get-cnt left) 1))))))

(defun insert-preserving-order (treap value)
  "valueが昇順ソートされた状態を保ったままvalueを挿入する O(log(n))
   insert, removeと併用すると壊れるため注意"
  (let ((pos (%find-pos treap value)))
    (insert treap pos value)))

(defun remove-preserving-order (treap value)
  "valueが昇順ソートされた状態を保ったままvalueを持つkeyを削除する O(log(n))
   insert, removeと併用すると壊れるため注意"
  ;; TODO valueが存在しないときにエラー
  (let ((pos (%find-pos treap value)))
    (remove treap pos)))

(defun ref (treap key)
  "treapのkeyに対応する値を返す。O(logN)"
  ;; TODO
  ;; - 汎変数 setf
  (multiple-value-bind (_removed center)
      (remove treap key)
    (declare (ignore _removed))
    (and center
         (treap-value center))))

#+swank (load (merge-pathnames "test/treap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
