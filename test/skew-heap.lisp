(ql:quickload :rove :slient t)

(defpackage :test/skew-heap
  (:use #:cl #:skew-heap)
  (:import-from #:rove #:testing #:ok #:deftest))

(in-package #:test/skew-heap)

(deftest skew-heap-test
  (testing "heap->list"
    (let ((xs (list (list 0 1)
                    (list 3 1)
                    (list 1 4)))
          (sk nil))
      (loop for (k v) in xs
            do (push! sk k v))
      (ok (equal '(0 1) (multiple-value-list (pop! sk))))
      (ok (equal '(1 4) (multiple-value-list (pop! sk))))
      (ok (equal '(3 1) (multiple-value-list (pop! sk)))))))

(rove:run-suite *package*)
