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

  (defun %get-template (template-name)
    (let ((template-form (gethash template-name *templates*)))
      (unless template-form
        (error "Template name ~a not found." template-name))
      template-form))

  (defun %add-form-to-template! (template-name name form &key doc)
    (when (gethash template-name *templates*)
      (warn "Redefing templat e: ~a" template-name))
    (pushnew (make-template-form :form form
                                 :name name
                                 :doc doc)
             (gethash template-name *templates*)
             :test #'equalp))

  (defun %eval-template! (template-name)
    (dolist (template-form (gethash template-name *templates*))
      (format *error-output* "Evaluating form: ~a" (template-form-name template-form))
      (eval (template-form-form template-form)))))

(defmacro add-form-to-template! ((template-name name) form &key doc)
  `(%add-form-to-template! ',template-name ',name ',form :doc ,doc))

(defmacro eval-template! (template-name)
  `(%eval-template! ,template-name))

#+nil
(deftemplate define-lseg (&key element-type result-type (key-element 'list)))


(add-form-to-template! lseg (element-type result-type))


(eval-template (element-type ))

(defstruct (clazy-segment-tree (:conc-name lseg-)
                               (:constructor %make-lseg (size )))
  (data (make-array )))
