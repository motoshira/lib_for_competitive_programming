(defpackage dynamic-segment-tree
  (:use #:cl)
  (:nicknames #:dseg)
  (:export #:make-dynamic-segtree
           #:build
           #:fold
           #:update!))

(in-package #:dynamic-segment-tree)

(defconstant +base+ 32)
(defconstant +max+ (ash 1 +base+))

(defstruct (node (:constructor %make-node (val p)))
  (value val :type t)
  (acc val :type t)
  (pos p :type fixnum)
  (l nil :type (or null node))
  (r nil :type (or null node)))

(defclass dynamic-segment-tree ()
  ((root :type (or null node)
         :accessor dseg-root
         :initarg :root
         :initform nil)
   (op :type t
       :accessor dseg-op
       :initarg :op
       :initform nil)))

;; Facade pattern

(defmethod fold ((dseg dynamic-segment-tree) l r)
  (%fold (dseg-root dseg) l r))

(defmethod update! ((dseg dynamic-segment-tree) key value)
  (setf (dseg-root dseg)
        (%update (dseg-root dseg) key value 0 +max+)))

(defun %get (node e)
  (if node (node-acc node) e))

(defun %fold (node l r ll rr op e)
  (cond
    ((or (null node)
         (<= rr l)
         (<= r ll))
     e)
    ((and (<= l ll)
          (<= rr r))
     (node-acc node))
    (t
     (let ((mid (ash (+ ll rr) -1)))
       (funcall op
                (funcall op
                         (%fold (node-l node)
                                l
                                r
                                ll
                                mid
                                op
                                e)
                         (node-value node))
                (%fold (node-r node)
                       l
                       r
                       mid
                       rr
                       op
                       e))))))

(defun %update (node l r value)
  nil)
