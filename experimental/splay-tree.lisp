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

(defun splay! (node)
  (symbol-macrolet ((p (node-parent node))
                    (pp (node-parent p))
                    (l (node-l node))
                    (r (node-r node)))
    (loop while p do
      (cond
        ((null pp)
         (if (eq (node-l p)
                 node)
             (zig! node :right)
             (zig! node :left)))
        ((eq (node-l p)
             node)
         (if (eq (node-l pp)
                 p)
             (zig-zig! node :right)
             (zig-zag! node :right)))
        (:else
         (if (eq (node-r pp)
                 p)
             (zig-zig! pp :left)
             (zig-zag! pp :left)))))))

(defun toggle (dir)
  (ecase dir
    (:left :right)
    (:right :left)))

(defun zig! (node dir)
  (symbol-macrolet ((p (node-parent node)))
    (ecase dir
      (:right (setf (node-l p) (node-r node)
                    (node-r node) p))
      (:left (setf (node-r p) (node-l node)
                   (node-l node) p)))))

(defun zig-zig! (node dir)
  ;; rotate parent -> rotate node
  (zig! (node-parent node) dir)
  (zig! node dir))

(defun zig-zag! (node dir)
  ;; dir is direction of rotate at the first time
  (zig! node dir)
  (zig! node (toggle dir)))
