(defpackage #:splay-tree
  (:use #:cl)
  (:nicknames #:sp))

(in-package #:splay-tree)

(defstruct (node (:constructor %make-node))
  (value 0 :type fixnum)
  (l nil :type (or null node))
  (r nil :type (or null node)))

(defstruct path
  (parent nil :type node)
  (p-dir nil :type (member :left :right))
  (pp nil :type (or null node))
  (pp-dir nil :type (or null (member :left :right))))

(defun parent-root-p (path)
  (null (path-pp path)))

(defstruct stack
  (data nil :type list))

;; TODO あとから最適化する

(defun stack-empty-p (stack)
  (null (stack-data stack)))

(defun pop! (stack)
  (prog1 (first (stack-data stack))
    (pop (stack-data stack))))

(defun push! (stack value)
  (push value (stack-data stack)))

(defun zig! (node parent p-dir)
  (ecase p-dir
    (:left (setf (node-l parent) (node-r node)
                 (node-r node) parent))
    (:right (setf (node-r parent) (node-l node)
                  (node-l node) parent))))

(defun zig-zig! (node parent pp p-dir)
  (ecase p-dir
    (:left (setf (node-l pp) (node-r parent)
                 (node-r parent) pp
                 (node-l parent) (node-r node)
                 (node-r node) parent))
    (:right (setf (node-r pp) (node-l parent)
                  (node-l parent) pp
                  (node-r parent) (node-l node)
                  (node-l node) parent))))

(defun zig-zag! (node parent pp p-dir)
  (ecase p-dir
    (:left (setf (node-r pp) (node-l node)
                 (node-l parent) (node-r node)
                 (node-l node) pp
                 (node-r node) parent))
    (:right (setf (node-l pp) (node-r node)
                  (node-r parent) (node-l node)
                  (node-r node) pp
                  (node-l node) parent))))

(defun splay! (node stack)
  (loop until (stack-empty-p stack)
        for path of-type path = (pop! stack)
        do (with-slots (parent p-dir pp pp-dir) path
             (if (parent-root-p path)
                 (zig! node parent p-dir)
                 (if (eq p-dir pp-dir)
                     (zig-zig! node parent pp p-dir)
                     (zig-zag! node parent pp p-dir))))))
