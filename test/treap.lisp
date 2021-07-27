(ql:quickload :rove :slient t)

(defpackage :test/treap
  (:use #:cl #:treap)
  (:import-from #:rove))

(in-package :test/treap)

#+swank
(rove:deftest test-treap
  (let* ((xs (loop repeat 5 collect (random 100)))
         (ys (loop repeat 10 collect (random 100)))
         (zs (loop repeat 20 collect (random 100)))
         (ws (list 1 3 5 7 10))  ; sum = 26
         (rs (list 1 5 4 3 2))   ; sum = 15
         (xs-treap (list->treap xs))
         (ys-treap (list->treap ys))
         (zs-treap (list->treap zs))
         (ws-treap (list->treap ws))
         (rs-treap (list->treap rs))
         (null-treap (list->treap nil)))
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
                      (concatenate 'list ws rs))
               "Merge two treap preserving order"))
    (rove:testing "split"
      (rove:ok (every #'equal
                      (mapcar #'treap->list
                              (multiple-value-list
                               (split ws-treap 2)))
                      '((1 3)
                        (5 7 10)))
               "Split at 2")
      (rove:ok (every #'equal
                      (mapcar #'treap->list
                              (multiple-value-list
                               (split ws-treap 3)))
                      '((1 3 5)
                        (7 10)))
               "Split at 3"))
    (rove:testing "insert"
      (rove:ok (equal (treap->list (insert ws-treap
                                           2
                                           11))
                      '(1 3 11 5 7 10))
               "Insert 11 at 2")
      (rove:ok (equal (treap->list (insert ws-treap
                                           3
                                           11))
                      '(1 3 5 11 7 10))
               "Insert 11 at 3"))
    (rove:testing "remove"
      (rove:ok (equal (treap->list (remove ws-treap 2))
                      '(1 3 7 10))))))

#+swank (rove:run-suite *package*)
