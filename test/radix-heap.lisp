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
    (let ((xs (list (list 1 2)
                    (list 2 3)
                    (list 3 4)
                    (list 34 8)
                    (list 60 60)))
          (heap (make-radix-heap)))
      (ok (empty-p heap))
      (loop for (k v) in xs
            do (push! heap k v))
      (ok (= 5 (heap-size heap)))
      (ok (equal '(1 2) (multiple-value-list (pop! heap))))
      (ok (equal '(2 3) (multiple-value-list (pop! heap))))
      (ok (equal '(3 4) (multiple-value-list (pop! heap))))
      (ok (equal '(34 8) (multiple-value-list (pop! heap))))
      (ok (equal '(60 60) (multiple-value-list (pop! heap))))
      (ok (zerop (heap-size heap))))))

#+swank (rove:run-suite *package*)
