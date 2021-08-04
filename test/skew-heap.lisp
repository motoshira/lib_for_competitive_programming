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
  (testing "meld"
    (let ((xs (list (list 1 2)
                    (list 0 4)))
          (ys (list (list 2 0)
                    (list 3 nil)))
          (xs-sk nil)
          (ys-sk nil))
      (loop for (k v) in xs
            do (setf xs-sk (meld xs-sk (sk::make-heap k v))))
      (loop for (k v) in ys
            do (setf ys-sk (meld ys-sk (sk::make-heap k v))))
      (ok (equalp '((0 4)
                    (1 2)
                    (2 0)
                    (3 nil))
                  (heap->list (meld xs-sk ys-sk))))))

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
            do (assert (numberp k))
            do (push! sk k v))
      (loop until (empty-p sk)
            for (k v) = (multiple-value-list (pop! sk))
            do (push (list k v) res))
      (assert (equalp (mapcar #'first (reverse res))
                      (sort (mapcar #'first xs) #'<))))))

(rove:run-suite *package*)
