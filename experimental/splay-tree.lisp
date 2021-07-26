(defpackage #:splay-tree
  (:use #:cl))

(in-package #:splay-tree)

(defvar *pointer-table* nil)

(defun init-table! ()
  (setf *pointer-table*
        (make-hash-table :test #'eq)))

(defmacro maybe (type) `(or null ,type))

(defstruct (pointer (:constructor %make-pointer (&optional (key (gensym "PTR")))))
  "uniqueなkeyを持つ"
  (key key :type symbol))


(defstruct node
  "splay-treeのnode"
  (value 0 :type fixnum)
  (ptr nil :type pointer)
  (l :none :type (maybe pointer))
  (r :none :type (maybe pointer)))

(declaim (ftype (function (pointer) (maybe node)) deref))
(defun deref (pointer)
  "nodeの実体を返す"
  (gethash (pointer-key pointer) *pointer-table*))

(declaim (ftype (function ((maybe node)) (maybe pointer)) ref))
(defun ref (node?)
  "nodeの参照を返す"
  (when node?
    (node-ptr node?)))
