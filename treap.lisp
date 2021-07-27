(in-package #:cl-user)

(defpackage #:treap
  (:use #:cl)
  (:shadow #:merge
           #:remove
           #:ref))

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
  ;; テスト済み
  (if (null treap)
      0
      (treap-cnt treap)))

(defun %get-sum (treap)
  ;; テスト済み
  (if (null treap)
      0
      (treap-sum treap)))

(defun %plus-cnt (l r)
  ;; テスト済み
  (+ (%get-cnt l)
     (%get-cnt r)))

(defun %plus-sum (l r)
  ;; テスト済み
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
  ;; FIXME:
  ;;   たまに壊れる
  ;;   splitの位置がずれてる？
  (cond
    ((null treap) (values nil nil))
    ((>= (%get-cnt (treap-l treap)) key)
     ;; cntが十分ある => 左
     (multiple-value-bind (new-l new-r)
         (split (treap-l treap)
                key)
       (let* ((r (merge (make-treap (treap-value treap)
                                    :sum (- (%get-sum treap)
                                            (%get-sum (treap-l treap))
                                            (treap-value treap))
                                    :cnt (- (%get-cnt treap)
                                            (%get-cnt (treap-l treap))
                                            1))
                        (treap-r treap)))
              (res-r (merge new-r r)))
         (values new-l res-r))))
    (:else
     ;; 右
     (let ((new-key (- key
                       (%get-cnt (treap-l treap)))))
       (multiple-value-bind (new-l new-r)
           (split (treap-r treap)
                  new-key)
         (let* ((l (merge (treap-l treap)
                          (make-treap (treap-value treap)
                                      :sum (- (%get-sum treap)
                                              (%get-sum (treap-r treap))
                                              (treap-value treap))
                                      :cnt (- (%get-cnt treap)
                                              (%get-cnt (treap-r treap))
                                              1))))
                (res-l (merge l
                              new-l)))
           (values res-l new-r)))))))

(defun insert (treap key value)
  ;; FIXME:
  ;;   たまに壊れる
  ;;   splitの位置がずれてる？
  (multiple-value-bind (l r)
      (split treap key)
    (merge (merge l (make-treap value :sum value))
           r)))

(defun remove (treap key)
  ;; FIXME: ky道がおかしい
  (multiple-value-bind (l c-r)
      (split treap (1- key))
    (multiple-value-bind (c r)
        (split c-r key)
      (let ((res (merge l r)))
        (values res c)))))

(defun ref (treap key)
  (multiple-value-bind (_removed center)
      (remove treap key)
    (declare (ignore _removed))
    (and center
         (treap-value center))))

#+swank
(defun list-equal (xs ys)
  "順番に関係なく要素が同じならOK O(n)"
  (let ((counter (make-hash-table :test #'eql)))
    (dolist (x xs)
      (incf (gethash x counter 0)))
    (dolist (y ys)
      (let ((cnt (gethash y counter 0)))
        (cond
          ((zerop cnt) (return-from list-equal nil))
          ((= 1 cnt) (remhash y counter))
          (:else (decf (gethash y counter))))))
    (zerop (hash-table-count counter))))

#+swank
(rove:deftest test-treap
  (let* ((xs (loop repeat 5 collect (random 100)))
         (ys (loop repeat 10 collect (random 100)))
         (zs (loop repeat 20 collect (random 100)))
         (ws (list 1 3 5 7 10)) ; sum = 26
         (rs (list 1 5 4 3 2))  ; sum = 15
         (xs-treap (list->treap xs))
         (ys-treap (list->treap ys))
         (zs-treap (list->treap zs))
         (ws-treap (list->treap ws))
         (rs-treap (list->treap rs))
         (null-treap (list->treap nil)))
    (rove:testing "list-equal"
      (rove:ok (list-equal '(5 1 4 2 3) rs))
      (rove:ok (list-equal '(1 2 4 3 5) rs))
      (rove:ok (not (list-equal '(1 1 5 4 3 2) rs)))
      (rove:ok (not (list-equal '(1 4 3 2) rs))))
    (rove:testing "Testing equality"
      (flet ((convert (list)
               (treap->list (list->treap list))))
        (rove:ok (equal (convert xs)
                        xs))))
    (rove:testing "get-cnt"
      (rove:ok (= (%get-cnt ys-treap)
                  10)
               "normal")
      (rove:ok (= (%get-cnt null-treap)
                  0)
               "null-treap"))
    (rove:testing "get-sum"
      (rove:ok (= (%get-sum ws-treap)
                  26))
      (rove:ok (= (%get-sum null-treap)
                  0)
               "null-treap"))
    (rove:testing "plus-cnt"
      (rove:ok (= (%plus-cnt xs-treap
                             ys-treap)
                  15)
               "normal")
      (rove:ok (= (%plus-cnt xs-treap
                             null-treap)
                  5)
               "one is null"))
    (rove:testing "plus-sum"
      (rove:ok (= (%plus-sum ws-treap rs-treap)
                  41)
               "normal")
      (rove:ok (= (%plus-sum ws-treap null-treap)
                  26)
               "one is null"))
    (rove:testing "merge"
      (rove:ok (equal (treap->list (merge ws-treap rs-treap))
                      (cl:merge 'list ws rs #'eql))))
    (rove:testing "split"
      (rove:ok (equalp (mapcar #'treap->list
                               (multiple-value-list
                                (split ws-treap 2)))
                       '((1 3)
                         (5 7 10)))))
    (rove:testing "insert"
      (rove:ok (equal (treap->list (insert ws-treap
                                           2
                                           11))
                      '(1 3 11 5 7 10))))
    #+nil
    (rove:testing "remove"
      (rove:ok (equal (treap->list (remove ws-treap 2))
                      '(1 3 7 10))))))

#+swank
(rove:run-suite *package*)

(in-package #:cl-user)
