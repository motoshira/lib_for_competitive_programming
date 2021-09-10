(ql:quickload :rove :slient t)

(defpackage :test/modint
  (:use #:cl)
  (:import-from #:modint)
  (:import-from #:rove #:testing #:ok #:deftest))

(in-package #:test/modint)

(deftest modint-test
  (testing "+"
    (ok (= (m:+ 1 2)
           3))))

(rove:run-suite *package*)
