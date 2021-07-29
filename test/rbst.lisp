(ql:quickload :rove :slient t)

(defpackage :test/rbst
  (:use #:cl #:rbst)
  (:shadowing-import-from #:rbst #:merge #:remove)
  (:import-from #:rove))

(in-package :test/rbst)

#+swank
(rove:deftest test-rbst
  (let* ((xs (loop repeat 5 collect (random 100)))
         (ys (loop repeat 10 collect (random 100)))
         (ws (list 1 3 5 7 10))  ; sum = 26
         (rs (list 1 5 4 3 2))   ; sum = 15
         (xs-rbst (list->rbst xs))
         (ys-rbst (list->rbst ys))
         (ws-rbst (list->rbst ws))
         (rs-rbst (list->rbst rs))
         (null-rbst (list->rbst nil)))
    (rove:testing "Testing equality"
      (flet ((convert (list)
               (rbst->list (list->rbst list))))
        (rove:ok (equal (convert xs)
                        xs))))
    (rove:testing "get-cnt"
      (rove:ok (= (rbst::%get-cnt ys-rbst)
                  10)
               "normal")
      (rove:ok (= (rbst::%get-cnt null-rbst)
                  0)
               "null-rbst"))
    (rove:testing "get-sum"
      (rove:ok (= (rbst::%get-sum ws-rbst)
                  26))
      (rove:ok (= (rbst::%get-sum null-rbst)
                  0)
               "null-rbst"))
    (rove:testing "plus-cnt"
      (rove:ok (= (rbst::%plus-cnt xs-rbst
                                    ys-rbst)
                  15)
               "normal")
      (rove:ok (= (rbst::%plus-cnt xs-rbst
                                    null-rbst)
                  5)
               "one is null"))
    (rove:testing "plus-sum"
      (rove:ok (= (rbst::%plus-sum ws-rbst rs-rbst)
                  41)
               "normal")
      (rove:ok (= (rbst::%plus-sum ws-rbst null-rbst)
                  26)
               "one is null"))
    (rove:testing "merge"
      (rove:ok (equal (rbst->list (merge ws-rbst rs-rbst))
                      '(1 3 5 7 10 1 5 4 3 2))
               "Merge two rbst preserving order"))
    (rove:testing "split"
      (rove:ok (every #'equal
                      (mapcar #'rbst->list
                              (multiple-value-list
                               (split ws-rbst 2)))
                      '((1 3)
                        (5 7 10)))
               "Split at 2")
      (rove:ok (every #'equal
                      (mapcar #'rbst->list
                              (multiple-value-list
                               (split ws-rbst 3)))
                      '((1 3 5)
                        (7 10)))
               "Split at 3"))
    (rove:testing "insert"
      (rove:ok (equal (rbst->list (insert ws-rbst
                                           2
                                           11))
                      '(1 3 11 5 7 10))
               "Insert 11 at 2")
      (rove:ok (equal (rbst->list (insert ws-rbst
                                           3
                                           11))
                      '(1 3 5 11 7 10))
               "Insert 11 at 3"))
    (rove:testing "remove"
      (rove:ok (equal (rbst->list (remove ws-rbst 2))
                      '(1 3 7 10))))
    (rove:testing "ref"
      (rove:ok (= (ref ws-rbst 2)
                  5))
      (rove:ok (= (ref ws-rbst 3)
                  7)))
    (rove:testing "insert-preserving-order"
      (rove:ok (equal (rbst->list (insert-preserving-order ws-rbst 0))
                      '(0 1 3 5 7 10))
               "0")
      (rove:ok (equal (rbst->list (insert-preserving-order ws-rbst 1))
                      '(1 1 3 5 7 10))
               "1")
      (rove:ok (equal (rbst->list (insert-preserving-order ws-rbst 2))
                      '(1 2 3 5 7 10))
               "2")
      (rove:ok (equal (rbst->list (insert-preserving-order ws-rbst 100))
                      '(1 3 5 7 10 100))
               "100"))
    (rove:testing "remove-preserving-order"
      (rove:ok (equal (rbst->list (remove-preserving-order ws-rbst 0))
                      '(3 5 7 10))
               "0"))
    (rove:testing "index out of range"
      (rove:ok
       (rove:signals (insert ws-rbst 6 10)
           'rbst::invalid-rbst-index-error)
       "insert, right of end")
      (rove:ok
       (rove:signals (insert null-rbst 1 10)
           'rbst::invalid-rbst-index-error)
       "insert, with null-rbst ")
      (rove:ok
       (rove:signals (remove ws-rbst 5)
           'rbst::invalid-rbst-index-error)
       "remove, right of end"))))

#+swank (rove:run-suite *package*)
