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
           #:remove))

(in-package  #:treap)

(deftype uint () '(integer 0 #.most-positive-fixnum))

(deftype maybe (type) `(or null ,type))

(defstruct (treap (:constructor make-treap (key &key (left nil) (right nil) (cnt 1) (sum value))))
  (key key :type fixnum)
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
                 (push (treap-key node)
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

(declaim (inline %propagate))
(defun %propagate (treap)
  ;; 子のcnt,sumが正しいことを前提とする
  ;; つまり葉から根へ伝搬すればよい
  (when treap
    (with-slots (left right) treap
      (setf (treap-cnt treap) (%plus-cnt left right))
      (setf (treap-sum treap) (%plus-sum left right)))))

(declaim (ftype (function ((maybe treap) (maybe treap)) (maybe treap)) merge))
(defun merge (l r)
  "２つのtreapを順序を保ったままマージする。O(logN)"
  ;; mergeに渡すtreapはpropagated
  ;; mergeから返ってくるtreapはpropagated
  (declare ((maybe treap) l r))
  (when (or (null l)
            (null r))
    (return-from merge (the (maybe treap)
                            (or l r))))
  (cond
    ((> (treap-priority l)
        (treap-priority r))
     ;; lが上
     (setf (treap-right l)
           (merge (treap-right l)
                  r))
     (%propagate l)
     l)
    (:else
     ;; rが上
     (setf (treap-left r)
           (merge l
                  (treap-left r)))
     (%propagate r)
     r)))

(declaim (ftype (function ((maybe treap) uint) (values (maybe treap) (maybe treap))) split))
(defun split (treap key)
  "treapを分割する。
   返り値: (values left right)

   left:  k未満のnodeからなるtreap
   right: k以上のnodeからなるtreap"
  ;; splitに渡すtreapはpropagated
  ;; splitから返ってくるtreapはpropagated
  (declare ((maybe treap) treap)
           (uint key))
  (when (null treap)
    (return-from split (values nil nil)))
  (the (values (maybe treap)
               (maybe treap))
       (cond
         ((>= (treap-key treap) key)
          ;; keyが十分大きい => 左

          (multiple-value-bind (new-l new-r)
              (split (treap-left treap)
                     key)
            (declare ((maybe treap) new-l new-r))
            (setf (treap-left treap) new-r)
            (%propagate treap)
            (values new-l treap)))
         (:else
          ;; 右
          (multiple-value-bind (new-l new-r)
              (split (treap-right treap)
                     key)
            (declare ((maybe treap) new-l new-r))
            (setf (treap-right treap) new-l)
            (%propagate treap)
            (values treap new-r))))))

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

(declaim (ftype (function ((maybe treap) fixnum) (maybe treap)) insert))
(defun insert (treap key)
  "keyを挿入する"
  (declare ((maybe treap) treap)
           (fixnum key))
  (multiple-value-bind (l r)
      (split treap key)
    (declare ((maybe treap) l r))
    (the (maybe treap)
         (merge (merge l (make-treap key))
                r))))

(declaim (ftype (function ((maybe treap) fixnum) (values (maybe treap) (maybe treap))) remove))
(defun remove (treap key)
  "keyを削除する"
  (declare ((maybe treap) treap)
           (fixnum key))
  (%check-index treap key :type :remove)
  (multiple-value-bind (l c-r)
      (split treap key)
    (declare ((maybe treap) l c-r))
    (multiple-value-bind (c r)
        (split c-r 1)
      (declare ((maybe treap) c r))
      (let ((res (merge l r)))
        (declare ((maybe treap) res))
        (values res c)))))

(define-modify-macro insert! (key) (lambda (treap key) (insert treap key)))
(define-modify-macro remove! (key) (lambda (treap key) (remove treap key)))

#+swank (load (merge-pathnames "test/treap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
