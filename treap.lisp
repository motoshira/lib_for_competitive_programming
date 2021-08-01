;;;
;;; BOF
;;;

;; Treap
;; Reference:
;;  「プログラミングコンテストでのデータ構造 2　～平衡二分探索木編～」
;;    https://www.slideshare.net/iwiwi/2-12188757
;;
;; TODO:
;;  - range-sum
;;  - range-op (RMQ, RUQ等に対応したい)

(defpackage #:treap
  (:use #:cl)
  (:nicknames #:tr)
  (:shadow #:merge
           #:remove
           #:first
           #:last)
  (:export #:list->treap
           #:treap->list
           #:merge
           #:split
           #:insert
           #:remove
           #:insert!
           #:remove!
           #:ref
           #:get-size
           #:count-value
           #:insert-value
           #:remove-value
           #:insert-value!
           #:remove-value!
           #:first
           #:last))

(in-package  #:treap)

(deftype uint () '(integer 0 #.most-positive-fixnum))

(deftype maybe (type) `(or null ,type))

(defstruct (treap (:constructor make-treap (value &key (left nil) (right nil) (cnt 1))))
  (left nil :type (or null treap))
  (right nil :type (or null treap))
  (value value :type fixnum)
  (priority (random #.most-positive-fixnum) :type uint)  ;; 勝手に決まる
  (cnt cnt :type uint))

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
              (merge treap (make-treap x)))
            xs
            :initial-value nil)))

(declaim (inline %get-cnt %plus-cnt))
(declaim (ftype (function ((maybe treap)) uint) %get-cnt))
(defun %get-cnt (treap)
  (declare ((maybe treap) treap))
  (the uint
       (if (null treap)
           0
           (treap-cnt treap))))

(declaim (inline get-size))
(defun get-size (treap)
  (%get-cnt treap))

(declaim (ftype (function ((maybe treap) (maybe treap)) uint) %plus-cnt))
(defun %plus-cnt (l r)
  (declare ((maybe treap) l r))
  (the uint
       (+ (%get-cnt l)
          (%get-cnt r))))

(declaim (inline %propagate))
(defun %propagate (treap)
  ;; 子のcntが正しいことを前提とする
  ;; つまり葉から根へ伝搬すればよい
  (when treap
    (with-slots (left right) treap
      (setf (treap-cnt treap) (1+ (%plus-cnt left right))))))

(declaim (ftype (function ((maybe treap) (maybe treap)) (maybe treap)) merge))
(defun merge (l r)
  "２つのtreapを順序を保ったままマージする。O(log(size))"
  ;; mergeに渡すtreapはpropagated
  ;; mergeから返ってくるtreapはpropagated
  (declare ((maybe treap) l r)
           (optimize (speed 3)))
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
   right: k以上のnodeからなるtreap
   O(log(size))"
  ;; splitに渡すtreapはpropagated
  ;; splitから返ってくるtreapはpropagated
  (declare (optimize (speed 3))
           ((maybe treap) treap)
           (uint key))
  (when (null treap)
    (return-from split (values nil nil)))
  (the (values (maybe treap)
               (maybe treap))
       (cond
         ((>= (%get-cnt (treap-left treap)) key)
          ;; cntが十分大きい => 左
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
                     (- key
                        (%get-cnt (treap-left treap))
                        1))
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
  #-swank (declare (ignore treap index type))
  #+swank
  (let ((end (ecase type
               (:insert (%get-cnt treap))
               (:remove (1- (%get-cnt treap))))))
    (unless (<= 0 index end)
      (error 'invalid-treap-index-error :begin 0
                                        :end end
                                        :index index))))

(declaim (ftype (function ((maybe treap) uint fixnum) (maybe treap)) insert))
(defun insert (treap key value)
  "keyの位置にvalueを挿入する。O(log(size))"
  (declare ((maybe treap) treap)
           (fixnum key value))
  (multiple-value-bind (l r)
      (split treap key)
    (declare ((maybe treap) l r))
    (the (maybe treap)
         (merge (merge l (make-treap value))
                r))))

(declaim (ftype (function ((maybe treap) fixnum) (values (maybe treap) (maybe treap))) remove))
(defun remove (treap key)
  "keyを削除する。O(log(size))"
  (declare ((maybe treap) treap)
           (uint key))
  (when (null treap)
    (error "Treap is empty."))
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

(define-modify-macro insert! (key value) (lambda (treap key value) (insert treap key value)) "keyの位置にvalueを挿入する。O(log(size))")
(define-modify-macro remove! (key) (lambda (treap key) (remove treap key)) "keyを削除する。O(log(size))")

(defmacro ref (treap key)
  "keyの値を返す。O(log(size))"
  (let ((removed (gensym "REMOVED"))
        (c (gensym "C"))
        (res (gensym "RES")))
    `(multiple-value-bind (,removed ,c)
         (remove ,treap ,key)
       (declare ((maybe treap) ,removed ,c))
       (let ((,res (when ,c (treap-value ,c))))
         (prog1 ,res
           ;; treapをもとに戻す
           (setf ,treap (if ,c
                            (insert ,removed ,key ,res)
                            ,removed)))))))

(declaim (ftype (function ((maybe treap) fixnum uint) uint) %find-pos))
(defun %find-pos (treap value acc)
  (declare (optimize (speed 3))
           ((maybe treap) treap)
           (fixnum value)
           (uint acc))
  (the uint
       (cond
         ((null treap) acc)
         ((= (treap-value treap) value)
          (+ acc
             (%get-cnt (treap-left treap))))
         ((> (treap-value treap) value)
          ;; 左
          (%find-pos (treap-left treap) value acc))
         (:else
          (let ((new-acc (+ acc
                            (%get-cnt (treap-left treap))
                            1)))
            (%find-pos (treap-right treap) value new-acc))))))

(declaim (ftype (function ((maybe treap) fixnum) (maybe treap)) insert-value remove-value))
(defun insert-value (treap value)
  "treapをmultisetとみなして値を追加する。insert/remove等と併用不可。O(log(size))"
  (let ((key (%find-pos treap value 0)))
    (declare (uint key))
    (insert treap key value)))

(defun remove-value (treap value)
  "treapをmultisetとみなして値を削除する。insert/remove等と併用不可。O(log(size))"
  (let ((key (%find-pos treap value 0)))
    (declare (uint key))
    (remove treap key)))

(define-modify-macro insert-value! (value)
  (lambda (treap value) (insert-value treap value))
  "treapをmultisetとみなして値を追加する。insert/remove等と併用不可。O(log(size))")
(define-modify-macro remove-value! (value)
  (lambda (treap value) (remove-value treap value))
  "treapをmultisetとみなして値を削除する。insert/remove等と併用不可。O(log(size))")

(declaim (ftype (function ((maybe treap)) fixnum) first last))
(defun first (treap)
  (declare ((maybe treap) treap))
  (ref treap 0))

(defun last (treap)
  (declare ((maybe treap) treap))
  (ref treap (1- (%get-cnt treap))))

(defun %upper-bound (treap value acc)
  "value以上を要素にもつnodeの中で最小のkeyを返す。なければnilを返す。"
  (when treap
    (with-slots (left
                 right
                 (tr-val value))
        treap
      (or (%upper-bound left value acc)
          (when (>= tr-val value)
            (+ acc
               (%get-cnt left)))
          (%upper-bound right value (+ acc
                                       (%get-cnt left)
                                       1))))))

(defmacro count-value (treap value)
  (let ((key (gensym))
        (key-plus-one (gensym))
        (c (gensym))
        (c-r (gensym))
        (l (gensym))
        (r (gensym))
        (size (gensym)))
    `(let* ((,key (%upper-bound ,treap ,value 0))
            (,key-plus-one (%upper-bound ,treap (1+ ,value) 0))
            (,size (%get-cnt ,treap)))
       (prog1 (- (or ,key-plus-one ,size)
                 (or ,key ,size))
         (multiple-value-bind (,l ,c-r)
             (if ,key
                 (split ,treap ,key)
                 (values ,treap nil))
           (multiple-value-bind (,c ,r)
               (if ,key-plus-one
                   (split ,c-r ,key-plus-one)
                   (values ,c-r nil))
             (setf ,treap
                   (merge (merge ,l ,c)
                          ,r))))))))

#+swank (load (merge-pathnames "test/treap.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)

;;;
;;; EOF
;;;
