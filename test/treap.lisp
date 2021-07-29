(ql:quickload :rove :slient t)

(defpackage :test/treap
  (:use #:cl #:treap)
  (:shadowing-import-from #:treap #:merge #:remove)
  (:import-from #:rove))

(in-package :test/treap)

#+swank
(rove:deftest test-treap
  nil)

#+swank (rove:run-suite *package*)
