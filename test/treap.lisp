(ql:quickload :rove :slient t)

(defpackage :test/treap
  (:use #:cl #:treap)
  (:shadowing-import-from #:treap #:merge #:remove)
  (:import-from #:rove))

(in-package :test/treap)

(defun make-fixture-from-list (list)
  (list->treap ))

#+swank
(rove:deftest test-treap
  (rove:testing "merge"
    (let* ((xs (list 1 4 3))
           (ys (list 3 7 9))
           (xs-tr (list->treap xs))
           (ys-tr (list->treap ys)))
      (rove:ok (equal (treap->list (merge xs-tr ys-tr))
                      '(1 4 3 3 7 9))))
    (let* ((xs (list 1 4 3))
           (ys (list 3 7 9))
           (xs-tr (list->treap xs))
           (ys-tr (list->treap ys)))
      (rove:ok (equal (treap->list (merge ys-tr xs-tr))
                      '(3 7 9 1 4 3))))))

#+swank (rove:run-suite *package*)
