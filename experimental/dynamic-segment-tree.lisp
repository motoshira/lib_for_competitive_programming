(defpackage dynamic-segment-tree
  (:use #:cl)
  (:nicknames #:dseg)
  (:export #:dynamic-segtree
           #:dynamic-segment-tree
           #:make-monoid
           #:build
           #:fold
           #:update!))

(in-package #:dynamic-segment-tree)

(defconstant +base+ 32)
(defconstant +max+ (ash 1 +base+))

(defstruct (node (:constructor %make-node (k v)))
  (key k :type fixnum)
  (value v :type t)
  (acc v :type t)
  (l nil :type (or null node))
  (r nil :type (or null node)))

(defstruct (monoid (:constructor make-monoid))
  (id nil :type t)
  (op nil :type (function (t t) boolean)))

(defclass dynamic-segment-tree ()
  ((root :type (or null node)
         :accessor dseg-root
         :initarg :root
         :initform nil)
   (monoid :type monoid
           :accessor dseg-monoid
           :initarg :monoid
           :initform (error "Initial value must be specified."))))

;; Facade pattern

(defmethod fold ((dseg dynamic-segment-tree) l r)
  (%fold (dseg-root dseg) l r 0 +max+ (dseg-monoid dseg)))

(defmethod update! ((dseg dynamic-segment-tree) key value)
  (setf (dseg-root dseg)
        (%update (dseg-root dseg) key value 0 +max+ (dseg-monoid dseg))))

(defmethod dump ((dseg dynamic-segment-tree))
  (%dump dseg))

(defun %dump (dseg)
  (with-slots (root monoid) dseg
    (let ((res nil))
      (sb-int:named-let rec ((node root))
        (when (node-l node)
          (rec (node-l node)))
        (with-slots (key value acc) node
          (push (list :key key
                      :value value
                      :acc acc)
                res))
        (when (node-r node)
          (rec (node-r node))))
      (reverse res))))

(defmethod print-object ((object dynamic-segment-tree)
                         stream)
  (print-unreadable-object (object stream)
    (fresh-line stream)
    (princ (dump object) stream)
    (fresh-line stream)))

(defun %get (node m)
  (if node (node-acc node) (monoid-id m)))

(defun %aggregate (l mid r m)
  (declare (fixnum l mid r))
  (with-slots (op) m
    (funcall op
             (funcall op
                      l
                      mid)
             r)))

(defun %update-acc! (node m)
  (declare (node node)
           (monoid m))
  (setf (node-acc node)
        (%aggregate (%get (node-l node) m)
                    (node-value node)
                    (%get (node-r node) m)
                    m)))

(defun %fold (node l r ll rr m)
  (with-slots (op id) m
    (cond
      ((or (null node)
           (<= rr l)
           (<= r ll))
       id)
      ((and (<= l ll)
            (<= rr r))
       (node-acc node))
      (t
       (let ((mid (ash (+ ll rr) -1)))
         (%aggregate (%fold (node-l node)
                            l
                            r
                            ll
                            mid
                            m)
                     (node-value node)
                     (%fold (node-r node)
                            l
                            r
                            mid
                            rr
                            m)
                     m))))))

(defun %update (node key value ll rr m)
  (with-slots (op id) m
    (if (null node)
        (%make-node key value)
        (let ((mid (ash (+ ll rr) -1)))
          (cond
            ((= (node-key node)
                key)
             (setf (node-value node) value)
             (%update-acc! node m)
             node)
            ((< (node-key node)
                mid)
             (when (> key (node-key node))
               (rotatef key (node-key node))
               (rotatef value (node-value node)))
             (setf (node-l node)
                   (%update (node-l node)
                            key
                            value
                            ll
                            mid
                            m))
             (%update-acc! node m)
             node)
            (t
             (when (< key (node-key node))
               (rotatef key (node-key node))
               (rotatef value (node-value node)))
             (setf (node-r node)
                   (%update (node-r node)
                            key
                            value
                            mid
                            rr
                            m))
             (%update-acc! node m)
             node))))))
