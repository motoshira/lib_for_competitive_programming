(ql:quickload '(:rove :sb-sprof) :slient t)

(defpackage :test/implicit-treap
  (:use #:cl #:implicit-treap)
  (:shadowing-import-from #:implicit-treap #:merge #:remove)
  (:import-from #:rove #:deftest #:testing #:ok)
  (:export #:run-bench
           #:run-and-profile))

(in-package :test/implicit-treap)

#+swank
(deftest test-itreap
  (rove:testing "merge"
    (let* ((xs (list 1 4 3))
           (ys (list 3 7 9))
           (zs (list 3 7 9))
           (xs-tr (list->itreap xs))
           (ys-tr (list->itreap ys))
           (zs-tr (list->itreap zs)))
      (rove:ok (equal (itreap->list (merge
                                     (merge xs-tr ys-tr)
                                     zs-tr))
                      '(1 4 3 3 7 9 3 7 9))))
    (let* ((xs (list 1 4 3))
           (ys (list 3 7 9))
           (xs-tr (list->itreap xs))
           (ys-tr (list->itreap ys)))
      (rove:ok (equal (itreap->list (merge ys-tr xs-tr))
                      '(3 7 9 1 4 3)))))
  (rove:testing "split"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->itreap xs)))
      (rove:ok (equalp (mapcar #'itreap->list
                               (multiple-value-list
                                (split xs-tr 2)))
                       '((1 4) (3 7 9)))))
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->itreap xs)))
      (rove:ok (equalp (mapcar #'itreap->list
                               (multiple-value-list
                                (split xs-tr 3)))
                       '((1 4 3) (7 9))))))
  (rove:testing "insert"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->itreap xs)))
      (rove:ok (equal (itreap->list
                       (insert xs-tr 5 10))
                      '(1 4 3 7 9 10))))
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->itreap xs)))
      (rove:ok (equal (itreap->list
                       (insert xs-tr 0 10))
                      '(10 1 4 3 7 9)))))
  (rove:testing "remove"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->itreap xs)))
      (rove:ok (equal (itreap->list
                       (remove xs-tr 0))
                      '(4 3 7 9)))))
  (rove:testing "remove!"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->itreap xs)))
      (remove! xs-tr 0)
      (rove:ok (equal (itreap->list xs-tr)
                      '(4 3 7 9)))
      (remove! xs-tr 3)
      (rove:ok (equal (itreap->list xs-tr)
                      '(4 3 7)))
      (remove! xs-tr 2)
      (rove:ok (equal (itreap->list xs-tr)
                      '(4 3)))))
  (rove:testing "ref"
    (let* ((xs (list 1 4 3 7 9))
           (xs-tr (list->itreap xs)))
      (rove:ok (= (ref xs-tr 0)
                  1))
      (rove:ok (equal (itreap->list xs-tr) xs))
      (rove:ok (= (ref xs-tr 2)
                  3))
      (rove:ok (equal (itreap->list xs-tr) xs))
      (rove:ok (= (ref xs-tr 1)
                  4))
      (rove:ok (equal (itreap->list xs-tr) xs))))
  (testing "range-update"
    (let* ((xs (list 1 5 2 4 3))
           (xs-tr (list->itreap xs)))
      (setf xs-tr (range-update xs-tr 0 2 10))
      (ok (equal '(10 10 2 4 3) (itreap->list xs-tr)))
      (setf xs-tr (range-update xs-tr 3 5 7))
      (ok (equal '(10 10 2 7 7) (itreap->list xs-tr)))
      (setf xs-tr (range-update xs-tr 0 4 0))
      (ok (equal '(0 0 0 0 7) (itreap->list xs-tr))
          "updateの範囲が被ってもOK")
      (remove! xs-tr 0)
      (ok (= 4 (get-size xs-tr))
          "cntが更新できている")
      (ok (equal '(0 0 0 7) (itreap->list xs-tr)))))
  (testing "fold"
    (let* ((xs (list 1 5 2 4 3))
           (xs-tr (list->itreap xs)))
      (ok (= 15 (fold xs-tr 0 5)))
      (update! xs-tr 0 0)
      (ok (= 14 (fold xs-tr 0 5)))
      (setf xs-tr (range-update xs-tr 1 5 0))
      (ok (= 0 (fold xs-tr 0 5)))
      (insert! xs-tr 0 10)
      (ok (= 10 (fold xs-tr 0 6)))
      (setf xs-tr (range-update xs-tr 0 3 20))
      (ok (= 60 (fold xs-tr 0 6))))))

#+swank
(defun test-run ()
  (dotimes (_ 30)
      (let ((itr nil))
        (declare ((maybe itreap) itr))
        (dotimes (i 10000)
          (insert! itr
                   (random (1+ i))
                   (random 10000000)))
        (dotimes (i 10000)
          (remove! itr 0)))))

#+swank
(defun run-bench ()
  (time
   (test-run)))

#+swank
(defun run-and-profile ()
  (sb-sprof:with-profiling (:max-samples 20
                            :report :flat)
    (test-run)))

#+swank (rove:run-suite *package*)
