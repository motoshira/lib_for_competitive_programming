(defpackage binomial-heap
  (:use #:cl)
  (:nicknames #:bh)
  (:shadow #:merge)
  (:export #:merge
           #:peek
           #:pop!
           #:push!))

(in-package #:binomial-heap)

(defstruct node
  key value level)

(defstruct heap
  subtrees)

(defun heap-root (heap)
  (first (heap-subtrees heap)))

(defun merge-tree (l-heap r-heap)
  (if (<= (heap-root l-heap)
          (heap-root r-heap))
      (add-subtree p q)
      (add-subtree q p)))

(defun add-subtree (l r)
  nil)

(defun merge (p q)
  ())
