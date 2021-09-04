(defpackage #:splay-tree
  (:use #:cl)
  (:nicknames #:sp)
  (:shadow #:merge))

(in-package #:splay-tree)

(defstruct (node (:constructor %make-node))
  (value 0 :type fixnum)
  (cnt 0 :type fixnum)
  (parent nil :type (or null node))
  (l nil :type (or null node))
  (r nil :type (or null node)))

(defun dump (node)
  (let ((res nil))
    (labels ((%traverse (node)
               (when node
                 (%traverse (node-l node))
                 (push (node-value node)
                       res)
                 (%traverse (node-r node)))))
      (%traverse node)
      (reverse res))))

(defmethod print-object ((obj node) s)
  (princ (dump obj) s))
