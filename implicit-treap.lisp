;;;
;;; BOF
;;;

;; Implicit Treap
;; Reference:
;;  「プログラミングコンテストでのデータ構造 2　～平衡二分探索木編～」
;;    https://www.slideshare.net/iwiwi/2-12188757
;;
;; TODO:
;;  - range-update
;;  - range-op (RMQ, RUQ等に対応したい)
;;  - range-sum (range-opがあればいらなさそう)

(defpackage #:implicit-treap
  (:use #:cl)
  (:nicknames #:itr)
  (:shadow #:merge
           #:remove
           #:first
           #:last)
  (:export #:list->itreap
           #:itreap->list
           #:implicit-treap
           #:maybe
           #:merge
           #:split
           #:insert
           #:remove
           #:update
           #:insert!
           #:remove!
           #:update!
           #:ref
           #:get-size
           #:fold))

(in-package  #:implicit-treap)

(deftype uint () '(integer 0 #.most-positive-fixnum))

(deftype maybe (type) `(or null ,type))

(defstruct (implicit-treap (:constructor make-itreap (value &key (left nil) (right nil) (cnt 1) (update-lazy 0) (is-ulazy nil)))
                           (:conc-name itreap-))
  (left nil :type (or null implicit-treap))
  (right nil :type (or null implicit-treap))
  (value value :type fixnum)
  (update-lazy update-lazy :type fixnum)
  (is-ulazy is-ulazy :type boolean)
  (priority (random #.most-positive-fixnum) :type uint)  ;; 勝手に決まる
  (cnt cnt :type uint))

(deftype itreap () 'implicit-treap)

(defun itreap->list (itreap)
  "itreapをlistに変換する。デバッグ用。O(n)"
  (let ((res nil))
    (labels ((%traverse (node)
               ;; 再帰的にpush
               (when node
                 (%traverse (itreap-left node))
                 (push (itreap-value node)
                       res)
                 (%traverse (itreap-right node)))))
      (%traverse itreap)
      (reverse res))))

#+swank
(defmethod print-object ((obj implicit-treap)
                         s)
  (print-unreadable-object (obj s :type t :identity t)
    (princ (itreap->list obj) s)))

(defun list->itreap (list)
  "listをitreapに変換する。デバッグ用。O(n)"
  (let ((xs (copy-seq list)))
    (reduce (lambda (itreap x)
              (merge itreap (make-itreap x)))
            xs
            :initial-value nil)))

#+swank (declaim (notinline %get-cnt %plus-cnt %update-cnt))
#-swank (declaim (inline %get-cnt %plus-cnt %update-cnt))
(declaim (ftype (function ((maybe itreap)) uint) %get-cnt))
(defun %get-cnt (itreap)
  (declare ((maybe itreap) itreap))
  (the uint
       (if (null itreap)
           0
           (itreap-cnt itreap))))

(declaim (inline get-size))
(defun get-size (itreap)
  (%get-cnt itreap))

(declaim (ftype (function ((maybe itreap) (maybe itreap)) uint) %plus-cnt))
(defun %plus-cnt (l r)
  (declare ((maybe itreap) l r))
  (the uint
       (+ (%get-cnt l)
          (%get-cnt r))))

(defun %update-cnt (itreap)
  (declare ((maybe itreap) itreap))
  (when itreap
    (with-slots (left right cnt) itreap
      (setf (the uint cnt)
            (the uint
                 (1+ (%plus-cnt left right)))))))

#+swank (declaim (notinline pushup))
#-swank (declaim (inline pushup))
(defun pushup (itreap)
  ;; 子のcntが正しいことを前提とする
  ;; つまり葉から根へ伝搬すればよい
  (declare ((maybe itreap) itreap))
  (%update-cnt itreap))

(declaim (#+swank notinline #-swank inline push-down))
(defun push-down (itreap)
  (declare ((maybe itreap)))
  (when itreap
    (with-slots (left right update-lazy is-ulazy) itreap
      (cond
        (is-ulazy
         ;; updateなのでほかはoverwriteする
         ;; TODO op-lazyもoverwrite
         (when left
           (setf (treap-update-lazy left) update-lazy
                 (treap-is-ulazy left) t))
         (when right
           (setf (treap-update-lazy right) update-lazy
                 (treap-is-ulazy right) t))
         ;; propagateし終わったので自分を更新して終わり
         (setf value update-lazy
               is-ulazy nil))))))

(declaim (ftype (function ((maybe itreap) (maybe itreap)) (maybe itreap)) merge))
(defun merge (l r)
  "２つのtreapを順序を保ったままマージする。O(log(size))"
  ;; mergeに渡すtreapはpropagated
  ;; mergeから返ってくるtreapはpropagated
  (declare ((maybe itreap) l r)
           (optimize (speed 3)))
  (push-down l)
  (push-down r)
  (when (or (null l)
            (null r))
    (return-from merge (the (maybe itreap)
                            (or l r))))
  (cond
    ((> (itreap-priority l)
        (itreap-priority r))
     ;; lが上
     (setf (itreap-right l)
           (merge (itreap-right l)
                  r))
     (pushup l)
     l)
    (:else
     ;; rが上
     (setf (itreap-left r)
           (merge l
                  (itreap-left r)))
     (pushup r)
     r)))

(declaim (ftype (function ((maybe itreap) uint) (values (maybe itreap) (maybe itreap))) split))
(defun split (itreap key)
  "treapを分割する。
   返り値: (values left right)

   left:  k未満のnodeからなるtreap
   right: k以上のnodeからなるtreap
   O(log(size))"
  ;; splitに渡すtreapはpropagated
  ;; splitから返ってくるtreapはpropagated
  (declare #+nil (optimize (speed 3))
           ((maybe itreap) itreap)
           (uint key))
  (push-down itreap)
  (when (null itreap)
    (return-from split (values nil nil)))
  (cond
    ((>= (%get-cnt (itreap-left itreap)) key)
     ;; cntが十分大きい => 左
     (multiple-value-bind (new-l new-r)
         (split (itreap-left itreap)
                key)
       (declare ((maybe itreap) new-l new-r))
       (setf (itreap-left itreap) new-r)
       (pushup itreap)
       (values new-l itreap)))
    (:else
     ;; 右
     (multiple-value-bind (new-l new-r)
         (split (itreap-right itreap)
                (the uint
                     (- key
                        (%get-cnt (itreap-left itreap))
                        1)))
       (declare ((maybe itreap) new-l new-r))
       (setf (itreap-right itreap) new-l)
       (pushup itreap)
       (values itreap new-r)))))

#+swank
(define-condition invalid-itreap-index-error (error)
  ((index :reader index :initarg :index)
   (begin :reader begin :initarg :begin)
   (end :reader end :initarg :end))
  (:report (lambda (condition stream)
             (with-slots (index begin end) condition
               (format stream "index must be (integer ~a ~a), not ~a." begin (1- end) index)))))

(defun %check-index (itreap index &key type)
  #-swank (declare (ignore treap index type))
  #+swank
  (let ((end (ecase type
               (:insert (%get-cnt itreap))
               (:remove (1- (%get-cnt itreap))))))
    (unless (<= 0 index end)
      (error 'invalid-itreap-index-error :begin 0
                                         :end end
                                         :index index))))

(declaim (ftype (function ((maybe itreap) uint fixnum) (maybe itreap)) insert))
(defun insert (itreap key value)
  "keyの位置にvalueを挿入する。O(log(size))"
  (declare ((maybe itreap) itreap)
           (uint key)
           (Fixnum value))
  (multiple-value-bind (l r)
      (split itreap key)
    (declare ((maybe itreap) l r))
    (the (maybe itreap)
         (merge (merge l (make-itreap value))
                r))))

(declaim (ftype (function ((maybe itreap) fixnum) (values (maybe itreap) (maybe itreap))) remove))
(defun remove (itreap key)
  "keyを削除する。O(log(size))"
  (declare ((maybe itreap) itreap)
           (uint key))
  (when (null itreap)
    (error "itreap is empty."))
  (%check-index itreap key :type :remove)
  (multiple-value-bind (l c-r)
      (split itreap key)
    (declare ((maybe itreap) l c-r))
    (multiple-value-bind (c r)
        (split c-r 1)
      (declare ((maybe itreap) c r))
      (let ((res (merge l r)))
        (declare ((maybe itreap) res))
        (values res c)))))

(declaim (ftype (function ((maybe itreap) uint fixnum) (maybe itreap)) update))
(defun update (itreap key value)
  (declare ((maybe itreap) itreap)
           (uint key)
           (fixnum value))
  (insert (remove itreap key)
          key
          value))

(define-modify-macro insert! (key value) (lambda (itreap key value) (insert treap key value)) "keyの位置にvalueを挿入する。O(log(size))")
(define-modify-macro remove! (key) (lambda (itreap key) (remove itreap key)) "keyを削除する。O(log(size))")
(define-modify-macro update! (key value) (lambda (itreap key value) (update itreap key value)) "keyの位置の値をvalueで更新する。O(log(size))")

(defmacro ref (itreap key)
  "keyの値を返す。O(log(size))"
  (let ((removed (gensym "REMOVED"))
        (c (gensym "C"))
        (res (gensym "RES")))
    `(multiple-value-bind (,removed ,c)
         (remove ,itreap ,key)
       (declare ((maybe itreap) ,removed ,c))
       (let ((,res (when ,c (itreap-value ,c))))
         (prog1 ,res
           ;; treapをもとに戻す
           (setf ,itreap (if ,c
                             (insert ,removed ,key ,res)
                             ,removed)))))))

#+swank (load (merge-pathnames "test/implicit-treap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
