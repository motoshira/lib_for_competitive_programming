(defpackage binomial-heap
  (:use #:cl)
  (:nicknames #:bh)
  (:shadow #:merge)
  (:export #:merge
           #:peek
           #:pop!
           #:push!))

(in-package #:binomial-heap)
