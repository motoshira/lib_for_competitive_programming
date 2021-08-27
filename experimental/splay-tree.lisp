(defpackage #:splay-tree
  (:use #:cl)
  (:nicknames #:sp)
  (:shadow #:merge))

(in-package #:splay-tree)

(defstruct (node (:constructor %make-node))
  (value 0 :type fixnum)
  (cnt 0 :type fixnum)
  (l nil :type (or null node))
  (r nil :type (or null node)))

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

(defun dump (node)
  (let ((res nil))
    (labels ((traverse (node)
               (when node
                 (traverse (node-l node))
                 (push (node-value node)
                       res)
                 (traverse (node-r node)))))
      (traverse node)
      (reverse res))))

(defun build (sequence)
  (let ((res nil))
    (map nil
         (lambda (x)
           (setf res
                 (%make-node :value x
                             :l res)))
         sequence)
    res))

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
        for (p p-dir) = (pop! stack)
        for (pp pp-dir) = (if stack
                              (pop! stack)
                              (list nil nil))
        do (if (null pp)
               (zig! node p p-dir)
               (if (eq p-dir pp-dir)
                   (zig-zig! node p pp p-dir)
                   (zig-zag! node p pp p-dir)))))

(defun %get-cnt (node)
  (if (null node)
      0
      (node-cnt node)))

(defun %update-cnt (node)
  (setf (node-cnt node)
        (+ 1
           (%get-cnt (node-l node))
           (%get-cnt (Node-r node)))))

(defun %push-up (node)
  (%update-cnt node))

(defun %find (node index stack)
  (when (null node)
    (return-from %find (values nil nil)))
  (%push-up node)
  (let ((cnt (%get-cnt (node-l node))))
    (cond
      ((= cnt index)
       (values node stack))
      ((< cnt index)
       (%find (node-l node)
              index
              (cons (list node :left)
                    stack)))
      (t
       (%find (node-r node)
              (- index cnt 1)
              (cons (list node :right)
                    stack))))))

(defun split (node index)
  (multiple-value-bind (target stack)
      (%find node index nil)
    (unless target
      (return-from split (values nil nil)))
    (splay! target stack)
    (let ((l (node-l target)))
      (setf (node-l target) nil)
      (values l target))))

(defun merge (l r)
  (multiple-value-bind (leftist stack)
      (%find l 0 nil)
    (unless leftist
      (return-from merge (values nil nil)))
    (splay! leftist stack)
    (assert (null (node-r leftist)))
    (setf (node-r leftist) r)
    leftist))

#+swank (load (merge-pathnames "test/splay-tree.lisp" (uiop:current-lisp-file-pathname)) :if-does-not-exist nil)
