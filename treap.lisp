(in-package #:cl-user)

(defpackage #:treap
  (:use #:cl)
  (:shadow #:merge
           #:remove
           #:ref)
  (:export #:list->treap
           #:treap->list
           #:%get-cnt
           #:%get-sum
           #:%plus-cnt
           #:%plus-sum
           #:merge
           #:split
           #:insert
           #:remove))

(in-package  #:treap)

(defstruct (treap (:constructor make-treap (value &key (l nil) (r nil) (cnt 1) (sum value))))
  (value value)
  (l nil :type (or null treap))
  (r nil :type (or null treap))
  (priority (random #.most-positive-fixnum))  ;; 勝手に決まる
  (cnt cnt)
  (sum sum))

(defun treap->list (treap)
  "デバッグ用。O(n)"
  (let ((res nil))
    (labels ((%traverse (node)
               ;; 再帰的にpush
               (when node
                 (%traverse (treap-l node))
                 (push (treap-value node)
                       res)
                 (%traverse (treap-r node)))))
      (%traverse treap)
      (reverse res))))

(defmethod print-object ((obj treap)
                         s)
  (print-unreadable-object (obj s :type t :identity t)
    (princ (treap->list obj) s)))

(defun list->treap (list)
  "デバッグ用。O(n)"
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
  (when (or (null l)
            (null r))
    (return-from merge (or l r)))
  (let ((new-cnt (%plus-cnt l r))
        (new-sum (%plus-sum l r)))
    (if (> (treap-priority l)
           (treap-priority r))
        ;; lが上
        (make-treap (treap-value l)
                    :l (treap-l l)
                    :r (merge (treap-r l)
                              r)
                    :cnt new-cnt
                    :sum new-sum)
        ;; rが上
        (make-treap (treap-value r)
                    :l (merge l
                              (treap-l r))
                    :r (treap-r r)
                    :cnt new-cnt
                    :sum new-sum))))

(defun split (treap key)
  "left:  k未満のnodeからなるtreap
   right: k以上のnodeからなるtreap"
  (cond
    ((null treap) (values nil nil))
    ((>= (%get-cnt (treap-l treap)) key)
     ;; cntが十分ある => 左
     (multiple-value-bind (new-l new-r)
         (split (treap-l treap)
                key)
       (let* ((r (merge (make-treap (treap-value treap)
                                    :sum (treap-value treap)
                                    :cnt 1)
                        (treap-r treap)))
              (res-r (merge new-r r)))
         (values new-l res-r))))
    (:else
     ;; 右
     (let ((new-key (- key
                       (%get-cnt (treap-l treap))
                       1)))
       (multiple-value-bind (new-l new-r)
           (split (treap-r treap)
                  new-key)
         (let* ((l (merge (treap-l treap)
                          (make-treap (treap-value treap)
                                      :sum (treap-value treap)
                                      :cnt 1)))
                (res-l (merge l new-l)))
           (values res-l new-r)))))))

(defun insert (treap key value)
  (multiple-value-bind (l r)
      (split treap key)
    (merge (merge l (make-treap value :sum value))
           r)))

(defun remove (treap key)
  (multiple-value-bind (l c-r)
      (split treap key)
    (multiple-value-bind (c r)
        (split c-r 1)
      (let ((res (merge l r)))
        (values res c)))))

(defun ref (treap key)
  (multiple-value-bind (_removed center)
      (remove treap key)
    (declare (ignore _removed))
    (and center
         (treap-value center))))

#+swank (load (merge-pathnames "test/treap.lisp" (truename ".")))
