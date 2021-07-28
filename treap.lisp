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
           #:remove
           #:ref)
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

(defstruct (treap (:constructor make-treap (value &key (left nil) (right nil) (cnt 1) (sum value))))
  (value value)
  (left nil :type (or null treap))
  (right nil :type (or null treap))
  (priority (random #.most-positive-fixnum))  ;; 勝手に決まる
  (cnt cnt)
  (sum sum))

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

(defun %get-cnt (treap)
  (if (null treap)
      0
      (treap-cnt treap)))

(defun %get-sum (treap)
  (if (null treap)
      0
      (treap-sum treap)))

(defun %plus-cnt (l r)
  (+ (%get-cnt l)
     (%get-cnt r)))

(defun %plus-sum (l r)
  (+ (%get-sum l)
     (%get-sum r)))

(defun merge (l r)
  "２つのtreapを順序を保ったままマージする。O(logN)"
  (when (or (null l)
            (null r))
    (return-from merge (or l r)))
  (let ((new-cnt (%plus-cnt l r))
        (new-sum (%plus-sum l r)))
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
                    :sum new-sum))))

(defun split (treap key)
  "treapを分割する。
   返り値: (values left right)

   left:  k未満のnodeからなるtreap
   right: k以上のnodeからなるtreap"
  (cond
    ((null treap) (values nil nil))
    ((>= (%get-cnt (treap-left treap)) key)
     ;; cntが十分ある => 左
     (multiple-value-bind (new-l new-r)
         (split (treap-left treap)
                key)
       (let* ((r (merge (make-treap (treap-value treap)
                                    :sum (treap-value treap)
                                    :cnt 1)
                        (treap-right treap)))
              (res-r (merge new-r r)))
         (values new-l res-r))))
    (:else
     ;; 右
     (let ((new-key (- key
                       (%get-cnt (treap-left treap))
                       1)))
       (multiple-value-bind (new-l new-r)
           (split (treap-right treap)
                  new-key)
         (let* ((l (merge (treap-left treap)
                          (make-treap (treap-value treap)
                                      :sum (treap-value treap)
                                      :cnt 1)))
                (res-l (merge l new-l)))
           (values res-l new-r)))))))

(defun insert (treap key value)
  "treapのkeyの位置にvalueを挿入する。O(logN)"
  (multiple-value-bind (l r)
      (split treap key)
    (merge (merge l (make-treap value :sum value))
           r)))

(defun remove (treap key)
  "treapのkeyの位置にあるvalueを削除する。O(logN)"
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
            (%find-insert-pos left value acc)
            (%find-insert-pos right value (+ acc (%get-cnt left) 1))))))

(defun insert-preserving-order (treap value)
  (let ((pos (%find-pos treap value)))
    (insert treap pos value)))

(defun remove-preserving-order (treap value)
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
