(defpackage dynamic-segment-tree
  (:use #:cl)
  (:nicknames #:dseg)
  (:export #:make-dynamic-segtree
           #:build
           #:fold
           #:update!))

(in-package #:dynamic-segment-tree)

(defstruct node
  (value nil :type t)
  (l nil :type (or null node))
  (r nil :type (or null node)))

(defclass dynamic-segment-tree ()
  ((root :type (or null node)
         :accessor dseg-root
         :initarg :root
         :initform nil)))

;; Facade pattern

(defmethod fold ((dseg dynamic-segment-tree) l r)
  (%fold (dseg-root dseg) l r))

(defmethod update! ((dseg dynamic-segment-tree) l r value)
  (setf (dseg-root dseg)
        (%update (dseg-root dseg) l r value)))

(defun %fold (node l r)
  nil)

(defun %update (node l r value)
  nil)
