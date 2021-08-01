(ql:quickload :rove :slient t)

(defpackage :test/treap
  (:use #:cl #:treap)
  (:shadowing-import-from #:treap #:merge #:remove #:first #:last)
  (:import-from #:rove))

(in-package :test/treap)

#+swank
(rove:deftest test-treap
  (rove:testing "merge"
    (let* ((xs (list 1 4 3))
           (ys (list 3 7 9))
           (zs (list 3 7 9))
           (xs-tr (list->treap xs))
           (ys-tr (list->treap ys))
           (zs-tr (list->treap zs)))
      (rove:ok (equal (treap->list (merge
                                    (merge xs-tr ys-tr)
                                    zs-tr))
                      '(1 4 3 3 7 9 3 7 9))))
    (let* ((xs (list 1 4 3))
           (ys (list 3 7 9))
           (xs-tr (list->treap xs))
           (ys-tr (list->treap ys)))
      (rove:ok (equal (treap->list (merge ys-tr xs-tr))
                      '(3 7 9 1 4 3)))))
  (rove:testing "split"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (equalp (mapcar #'treap->list
                               (multiple-value-list
                                (split xs-tr 2)))
                       '((1 4) (3 7 9)))))
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (equalp (mapcar #'treap->list
                               (multiple-value-list
                                (split xs-tr 3)))
                       '((1 4 3) (7 9))))))
  (rove:testing "insert"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (equal (treap->list
                       (insert xs-tr 5 10))
                      '(1 4 3 7 9 10))))
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (equal (treap->list
                       (insert xs-tr 0 10))
                      '(10 1 4 3 7 9)))))
  (rove:testing "remove"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (equal (treap->list
                       (remove xs-tr 0))
                      '(4 3 7 9)))))
  (rove:testing "remove!"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->treap xs)))
      (remove! xs-tr 0)
      (rove:ok (equal (treap->list xs-tr)
                      '(4 3 7 9)))
      (remove! xs-tr 3)
      (rove:ok (equal (treap->list xs-tr)
                      '(4 3 7)))
      (remove! xs-tr 2)
      (rove:ok (equal (treap->list xs-tr)
                      '(4 3)))))
  (rove:testing "ref"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (= (ref xs-tr 0)
                  1))
      (rove:ok (equal (treap->list xs-tr) xs))
      (rove:ok (= (ref xs-tr 2)
                  3))
      (rove:ok (equal (treap->list xs-tr) xs))
      (rove:ok (= (ref xs-tr 1)
                  4))
      (rove:ok (equal (treap->list xs-tr) xs))))
  (rove:testing "count-value"
    (let* ((xs (list 1 1 3 4 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (= 1 (count-value xs-tr 3)))
      (rove:ok (= 2 (count-value xs-tr 1)))))
  (rove:testing "insert-value"
    (let* ((xs (list 1 3 4 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (equal (treap->list
                       (insert-value
                        xs-tr 0))
                      '(0 1 3 4 7 9))))
    (let* ((xs (list 1 3 4 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (equal (treap->list
                       (insert-value
                        xs-tr 10))
                      '(1 3 4 7 9 10)))))
  (rove:testing "remove-value"
    (let* ((xs (list 1 3 4 7 9))
           (xs-tr (list->treap xs)))
      (setf xs-tr (remove-value
                   xs-tr 1))
      (rove:ok (equal (treap->list xs-tr)
                      '(3 4 7 9)))
      (setf xs-tr (remove-value
                   xs-tr 4))
      (rove:ok (equal (treap->list xs-tr)
                      '(3 7 9)))))
  (rove:testing "insert-value!"
    (let* ((xs (list 1 3 4 7 9))
           (xs-tr (list->treap xs)))
      (insert-value! xs-tr 0)
      (rove:ok (equal (treap->list xs-tr)
                      '(0 1 3 4 7 9)))
      (insert-value! xs-tr 9)
      (rove:ok (equal (treap->list xs-tr)
                      '(0 1 3 4 7 9 9)))
      (insert-value! xs-tr 5)
      (rove:ok (equal (treap->list xs-tr)
                      '(0 1 3 4 5 7 9 9)))))
  (rove:testing "remove-value!"
    (let* ((xs (list 1 3 4 7 9))
           (xs-tr (list->treap xs)))
      (remove-value! xs-tr 1)
      (rove:ok (equal (treap->list xs-tr)
                      '(3 4 7 9)))
      (remove-value! xs-tr 4)
      (rove:ok (equal (treap->list xs-tr)
                      '(3 7 9)))
      (remove-value! xs-tr 9)
      (rove:ok (equal (treap->list xs-tr)
                      '(3 7)))))
  (rove:testing "first"
    (let* ((xs (list 1 3 4 7 9))
           (xs-tr (list->treap xs)))
      (rove:ok (= (first xs-tr)
                  1)))))


#+swank (rove:run-suite *package*)
