(defpackage lazy-segment-tree
  (:use #:cl)
  (:nicknames #:lseg)
  (:export #:make-lseg))

(in-package #:lseg)


(eval-when (:compile-toplevel :load-toplevel)
  (defvar *templates* (make-hash-table :test #'eq))
  (defstruct (template-form (:type list)
                            (:conc-name tform-))
    (form nil :type cons)
    (name nil :type symbol)
    (doc nil :type (or null string)))

  (defun %get-template (name)
    (let ((template-form (gethash name *templates*)))
      (unless template-form
        (error "Template name ~a not found." name))
      template-form))

  (defun %add-template! (name form &key doc)
    (when (gethash name *templates*)
      (warn "Redefing template: ~a" name))
    (setf (gethash name *templates*)
          (make-template-form :form form
                              :name name
                              :doc doc))))

(defmacro add-template! (name form &key doc)
  `(%add-template! ',name ',form :doc ,doc))

#+nil
(deftemplate define-lseg (&key element-type result-type (key-element 'list)))


(add-form-to-template! lseg (element-type result-type))


(eval-template (element-type ))

(defstruct (clazy-segment-tree (:conc-name lseg-)
                               (:constructor %make-lseg (size )))
  (data (make-array )))
