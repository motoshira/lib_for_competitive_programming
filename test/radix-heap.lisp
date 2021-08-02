(ql:quickload :rove :slient t)

(defpackage test/radix-heap
  (:use #:cl #:radix-heap)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest))

(in-package #:test/radix-heap)

(deftest radix-heap-test
  (testing "pair-stack"
    (let* ((xs (list 1 5 4 2 3))
           (pair-stack nil))
      (dolist (x xs)
        (let ((y (1+ x)))
          (rd::pstack-push! (x y) pair-stack)))
      (setf xs (reverse xs))
      (rd::do-pstack ((y z) pair-stack)
        (let ((x (pop xs)))
          (ok (equal (list x (1+ x))
                     (list y z)))))))
  (testing "radix-heap"
    (let ((xs (list 5 2 1 3 4))
          (heap (make-radix-heap)))
      (ok
       (empty-p heap)))))

#+swank (rove:run-suite *package*)
