(ql:quickload :rove :slient t)

(defpackage :test/modint
  (:use #:cl)
  (:import-from #:modint)
  (:import-from #:rove #:testing #:ok #:deftest))

(in-package #:test/modint)

(deftest modint-test
  (testing "+"
    (ok (= (m:+ 1 (- m:*mod* 2))
           (- m:*mod* 1)))
    (ok (= (m:+ (1- m:*mod*)
                1)
           0))
    (ok (= (m:+ 10)
           10)))
  (testing "-"
    (ok (= (m:- m:*mod* 1)
           (- m:*mod* 1)))
    (ok (= (m:- 1 2)
           (1- m:*mod*)))
    (ok (= (m:- 1)
           1)))
  (testing "*"
    (ok (= (m:* (ash m:*mod* -1) 2)
           (1- m:*mod*)))
    (ok (= (m:* 3 0)
           0))
    (ok (= (m:* 0 3)
           0)))
  (testing "mod-inv"
    (flet ((%get-one (x)
             (m:* x (m:mod-inv x))))
      (ok (= (%get-one 1) 1))
      (ok (= (%get-one (1- m:*mod*)) 1))))
  (testing "/"
    (flet ((f (x y)
             (m:* (m:/ x y) y)))
      (ok (= (f 10 3)
             10)))))

(rove:run-suite *package*)
