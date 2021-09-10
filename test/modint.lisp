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
      (ok (= (%get-one 3) 1))
      (ok (= (%get-one (1- m:*mod*)) 1))))
  (testing "/"
    (flet ((f (x y)
             (m:* (m:/ x y) y)))
      (ok (= (f 10 3)
             10))
      (ok (= (f 100 3)
             100))
      (ok (= (f 1 3)
             1))
      (ok (= (f (1- m:*mod*) 3)
             (1- m:*mod*)))))
  (testing "mod-pow"
    (ok (= (m:mod-power 10 2)
           100)
        (= (m:mod-power 10 100)
           (reduce #'m:* (loop repeat 100 collect 10)))))
  (testing "mod-fact and combi"
    (let ((table (m:make-mod-fact-table 101))
          (get-fact (lambda (x)
                      (reduce #'m:*
                              (loop for x from 1 to x
                                    collect x))))
          (calc-combi (lambda (n k)
                        (let ((tmp 1))
                          (loop for x from 1 to k
                                for y downfrom n
                                do (m:mulmodf tmp y)
                                   (m:divmodf tmp x))
                          tmp))))
      (testing "mod-fact"
        (ok (= (aref table 0)
               1))
        (ok (= (aref table 10)
               (funcall get-fact 10)))
        (ok (= (aref table 100)
               (funcall get-fact 100))))
      (testing "mod-combi"
        (ok (= (m:mod-combi-with-table 10 0 table)
               1))
        (ok (= (m:mod-combi-with-table 10 1 table)
               10))
        (ok (= (m:mod-combi-with-table 10 2 table)
               45))
        (ok (= (m:mod-combi-with-table 100 30 table)
               (funcall calc-combi 100 30)))))))

(rove:run-suite *package*)
