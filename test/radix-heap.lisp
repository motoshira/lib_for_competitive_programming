(ql:quickload :rove :slient t)

(defpackage test/radix-heap
  (:use #:cl #:radix-heap)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest))

(in-package #:test/radix-heap)
