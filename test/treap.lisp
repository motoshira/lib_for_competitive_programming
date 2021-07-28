(ql:quickload :rove :slient t)

(defpackage :test/treap
  (:use #:cl #:treap)
  (:shadowing-import-from #:treap #:merge #:remove)
  (:import-from #:rove))

(in-package :test/treap)

#+swank
(rove:deftest test-treap
  (let* ((xs (loop repeat 5 collect (random 100)))
         (ys (loop repeat 10 collect (random 100)))
         (ws (list 1 3 5 7 10))  ; sum = 26
         (rs (list 1 5 4 3 2))   ; sum = 15
         (xs-treap (list->treap xs))
         (ys-treap (list->treap ys))
         (ws-treap (list->treap ws))
         (rs-treap (list->treap rs))
         (null-treap (list->treap nil)))
    (rove:testing "Testing equality"
      (flet ((convert (list)
               (treap->list (list->treap list))))
        (rove:ok (equal (convert xs)
                        xs))))
    (rove:testing "get-cnt"
      (rove:ok (= (treap::%get-cnt ys-treap)
                  10)
               "normal")
      (rove:ok (= (treap::%get-cnt null-treap)
                  0)
               "null-treap"))
    (rove:testing "get-sum"
      (rove:ok (= (treap::%get-sum ws-treap)
                  26))
      (rove:ok (= (treap::%get-sum null-treap)
                  0)
               "null-treap"))
    (rove:testing "plus-cnt"
      (rove:ok (= (treap::%plus-cnt xs-treap
                                    ys-treap)
                  15)
               "normal")
      (rove:ok (= (treap::%plus-cnt xs-treap
                                    null-treap)
                  5)
               "one is null"))
    (rove:testing "plus-sum"
      (rove:ok (= (treap::%plus-sum ws-treap rs-treap)
                  41)
               "normal")
      (rove:ok (= (treap::%plus-sum ws-treap null-treap)
                  26)
               "one is null"))
    (rove:testing "merge"
      (rove:ok (equal (treap->list (merge ws-treap rs-treap))
                      '(1 3 5 7 10 1 5 4 3 2))
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
                      '(1 3 7 10))))
    (rove:testing "ref"
      (rove:ok (= (ref ws-treap 2)
                  5))
      (rove:ok (= (ref ws-treap 3)
                  7)))
    (rove:testing "insert-preserving-order"
      (rove:ok (equal (treap->list (insert-preserving-order ws-treap 0))
                      '(0 1 3 5 7 10))
               "0")
      (rove:ok (equal (treap->list (insert-preserving-order ws-treap 1))
                      '(1 1 3 5 7 10))
               "1")
      (rove:ok (equal (treap->list (insert-preserving-order ws-treap 2))
                      '(1 2 3 5 7 10))
               "2")
      (rove:ok (equal (treap->list (insert-preserving-order ws-treap 100))
                      '(1 3 5 7 10 100))
               "100"))
    (rove:testing "remove-preserving-order"
      (rove:ok (equal (treap->list (remove-preserving-order ws-treap 0))
                      '(3 5 7 10))
               "0"))
    (rove:testing "index out of range"
      (rove:ok
       (rove:signals (insert ws-treap -1 10)
           'treap::invalid-treap-index-error)
       "insert, left of begin")
      (rove:ok
       (rove:signals (insert ws-treap 6 10)
           'treap::invalid-treap-index-error)
       "insert, right of end")
      (rove:ok
       (rove:signals (insert null-treap 1 10)
           'treap::invalid-treap-index-error)
       "insert, with null-treap ")
      (rove:ok
       (rove:signals (remove ws-treap 5)
           'treap::invalid-treap-index-error)
       "remove, right of end"))))

#+swank (rove:run-suite *package*)
