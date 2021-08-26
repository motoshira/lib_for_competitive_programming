(ql:quickload :rove :slient t)

(defpackage test/splay-tree
  (:use #:cl
        #:splay-tree)
  (:import-from #:rove
                #:deftest
                #:testing
                #:ok))

(in-package #:test/splay-tree)

(deftest splay-tree-test
  (testing "pass"
    (ok t)))

(rove:run-suite *package*)
