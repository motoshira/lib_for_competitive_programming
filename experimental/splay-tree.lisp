(defpackage #:splay-tree
  (:use #:cl)
  (:nicknames #:sp))

(in-package #:splay-tree)

(defstruct (node (:constructor %make-node))
  (value 0 :type fixnum)
  (l nil :type (or null node))
  (r nil :type (or null node)))
