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
      (ok (equal '(3 1) (multiple-value-list (pop! sk))))))
  (testing "empty-p"
    (ok (empty-p nil))
    (ok (not (empty-p (sk::make-heap 1 1)))))
  (testing "peak"
    (ok (equal '(1 2) (multiple-value-list (peak (sk::make-heap 1 2)))))
    (ok (equal '(nil nil) (multiple-value-list (peak nil)))))
  #+nil
  (testing "push!"
    (let ((xs (loop repeat 10000
                    for k = (random 1000000)
                    for v = (nth (random 3)
                                 (list (random 1000000)
                                       t
                                       nil))
                    collect (list k v)))
          (sk nil)
          (res nil))
      (loop for (k v) in xs
            do (assert (numberp ))
            do (push! sk k v))
      (loop until (empty-p sk)
            for (values k v) = (pop! sk)
            do (push (list k v) res))
      (assert (equalp (mapcar #'first (reverse res))
                      (sort (mapcar #'first xs) #'<))))))

(rove:run-suite *package*)
