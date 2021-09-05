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

(defun %get-cnt (node)
  (if (null node)
      0
      (node-cnt node)))

(defun %update-cnt! (node)
  (setf (node-cnt node)
        (+ (%get-cnt (node-l node))
           (%get-cnt (node-r node))
           1)))

(defun %push-up! (node)
  (%update-cnt! node))

(defun splay (node)
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
             (zig-zag! pp :left)))))
    node))

(defun toggle (dir)
  (ecase dir
    (:left :right)
    (:right :left)))

(defun zig! (node dir)
  (symbol-macrolet ((p (node-parent node)))
    (ecase dir
      (:right (progn
                (setf (node-l p) (node-r node))
                (%push-up! p)
                (setf (node-r node) p)
                (%push-up! node)))
      (:left (progn
               (setf (node-r p) (node-l node))
               (%push-up! p)
               (setf (node-l node) p)
               (%push-up! node))))))

(defun zig-zig! (node dir)
  ;; rotate parent -> rotate node
  (zig! (node-parent node) dir)
  (zig! node dir))

(defun zig-zag! (node dir)
  ;; dir is direction of rotate at the first time
  (zig! node dir)
  (zig! node (toggle dir)))

#+nil
(defun lower-bound (node value &optional (acc 0))
  "Get max value x that satisfy: (<= (node-value (ref node x)) value)"
  (assert node)
  (if (<= (node-value node)
          value)
      (if (node-l node)
          (lower-bound (node-l node)
                       value
                       (+ acc
                          ()))
          )))

(defun %find-right-end (node)
  (if (null (node-r node))
      node
      (%find-right-end (node-r node))))

(defun merge (l r)
  (let ((l (splay (%find-right-end l))))
    (assert (null (node-r l)))
    (setf (node-r l) r)
    l))

(defun %find (node key)
  (let ((l-cnt (if (node-l node)
                   (node-cnt (node-l node))
                   0)))
    (cond
      ((= key l-cnt) node)
      ((< key l-cnt)
       (%find (node-l node) key))
      (:else
       (%find (node-r node) (- key l-cnt 1))))))

(defun split (node key)
  (let* ((k-th (splay (%find node key)))
         (r (node-r k-th)))
    (setf (node-r k-th) nil)
    (values k-th r)))
