;;;
;;; BOF
;;;

;; Dynamic Segment Tree
;; Reference:
;;  「動的なSegment Treeのテクニック」
;;  https://kazuma8128.hatenablog.com/entry/2018/11/29/093827

(defpackage dynamic-segment-tree
  (:use #:cl)
  (:nicknames #:dseg)
  (:export #:dynamic-segment-tree
           #:make-dynamic-segment-tree
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

(defstruct (monoid (:constructor %make-monoid))
  (id nil :type t)
  (op nil :type (function (t t) t)))

(defclass dynamic-segment-tree ()
  ((root :type (or null node)
         :accessor dseg-root
         :initarg :root
         :initform nil)
   (monoid :type monoid
           :accessor dseg-monoid
           :initarg :monoid
           :initform (error "Initial value must be specified."))))

;; Public

(defun make-dynamic-segment-tree (op identity)
  (make-instance 'dynamic-segment-tree
                  :monoid (%make-monoid :op op
                                        :id identity)))

#-swank (declaim (inline fold update!))
(defmethod fold ((dseg dynamic-segment-tree) l r)
  (%fold (dseg-root dseg) l r 0 +max+ (dseg-monoid dseg)))

(defmethod update! ((dseg dynamic-segment-tree) key value)
  (setf (dseg-root dseg)
        (%update (dseg-root dseg) key value 0 +max+ (dseg-monoid dseg)))
  dseg)

(defmethod ref ((dseg dynamic-segment-tree) key)
  (fold dseg key (1+ key)))

(defun (setf ref) (value dseg key)
  (update! dseg key value))

;; Private

(defmethod dump ((dseg dynamic-segment-tree))
  (%dump dseg))

(defun %dump (dseg)
  (with-slots (root monoid) dseg
    (let ((res nil))
      (sb-int:named-let rec ((node root))
        (when node
          (when (node-l node)
            (rec (node-l node)))
          (with-slots (key value acc) node
            (push (list :key key
                        :value value
                        :acc acc)
                  res))
          (when (node-r node)
            (rec (node-r node)))))
      (reverse res))))

(defmethod print-object ((object dynamic-segment-tree)
                         stream)
  (print-unreadable-object (object stream)
    (fresh-line stream)
    (princ (dump object) stream)
    (fresh-line stream)))

#-swank (declaim (inline %get))
(defun %get (node m)
  (declare ((or null node) node)
           (monoid m))
  (if node (node-acc node) (monoid-id m)))

#-swank (declaim (inline %aggregate))
(defun %aggregate (l mid r m)
  (declare (fixnum l mid r)
           (monoid m))
  (with-slots (op) m
    (funcall op
             (funcall op
                      l
                      mid)
             r)))

#-swank (declaim (inline %update-acc!))
(defun %update-acc! (node m)
  (declare (node node)
           (monoid m))
  (setf (node-acc node)
        (%aggregate (%get (node-l node) m)
                    (node-value node)
                    (%get (node-r node) m)
                    m)))

(defun %fold (node l r ll rr m)
  (declare ((or null node) node)
           (fixnum l r ll rr)
           (monoid m))
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
       (let ((mid (ash (+ ll rr) -1))
             (res id))
         (declare (fixnum mid))
         (setf res (funcall op
                            res
                            (%fold (node-l node)
                                   l
                                   r
                                   ll
                                   mid
                                   m)))
         (when (<= l (node-key node) (1- r))
           (setf res (funcall op
                              res
                              (node-value node))))
         (setf res (funcall op
                            res
                            (%fold (node-r node)
                                   l
                                   r
                                   mid
                                   rr
                                   m)))
         res)))))

(defun %update (node key value ll rr m)
  (declare ((or null node) node)
           (fixnum key ll rr)
           (t value)
           (monoid m))
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

;;;
;;; EOF
;;;
