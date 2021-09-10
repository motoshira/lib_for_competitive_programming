;;;
;;; Beginning of inserted contents
;;;

;; modint functions

(defpackage #:modint
  (:use #:cl)
  (:nicknames #:m #:mod)
  (:shadow #:+
           #:-
           #:*
           #:/)
  (:export #:modint
           #:+
           #:-
           #:*
           #:/
           #:mod-inv
           #:*mod*))

(in-package #:modint)

(define-symbol-macro *mod* 1000000007)
;; (define-symbol-macro *mod* 998244353)

(declaim (inline modint)
         (ftype (function (integer) fixnum) modint))
(defun modint (integer)
  ;; (integer) -> (fixnum)
  ;; TODO TLE on big negative integer
  (declare (integer integer))
  (loop while (minusp integer)
        do (incf integer *mod*))
  (the fixnum
       (if (< integer *mod*)
           integer
           (mod integer *mod*))))

(defun %operate (fn &rest args)
  (declare ((function (fixnum fixnum) fixnum) fn))
  (if (null args)
      0
      (reduce (lambda (x y)
                (modint (funcall fn x (modint y))))
              (rest args)
              :initial-value (first args))))

(defun + (&rest args)
  (%operate cl:+ args))

(defun - (&rest args)
  (%operate cl:- args))

(defun * (&rest args)
  (%operate cl:* args))

(defun / (&rest args)
  (%operate (lambda (x y)
              (* x (mod-inv y)))
            args))

(defmacro %expand (fn x y)
  `(modint (funcall (the (function (fixnum fixnum) fixnum)
                         ,fn)
                    ,x
                    (modint ,y))))

(declaim (ftype (function (fixnum) fixnum) mod-inv))
(defun mod-inv (a)
  "Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a"
  (declare (fixnum a)
           (optimize (speed 3)))
  (let ((b *mod*)
        (u 1)
        (v 0))
    (declare (fixnum b u v))
    (loop until (zerop b) do
      (let ((w (truncate a b)))
        (decf (the fixnum a) (the fixnum
                                  (* (the fixnum w)
                                     (the fixnum b))))
        (rotatef (the fixnum a)
                 (the fixnum b))
        (decf (the fixnum u)
              (the fixnum
                   (* (the fixnum w)
                      (the fixnum v))))
        (rotatef u v)))
    (modint u)))

(declaim (ftype (function (fixnum (integer 0)) fixnum) mod-power)
         (inline mod-power))
(defun mod-power (base power)
  ;; Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a
  (declare (fixnum base)
           ((integer 0) power))
  (loop while (plusp power)
        with res of-type fixnum = 1
        do (psetq base (the fixnum (* base base))
                  power (the (integer 0) (ash power -1))
                  res (the fixnum (if (logbitp 0 power)
                                    (* res base)
                                    res)))
        finally (return res)))

(define-modify-macro incmodf (&optional (val 1)) (lambda (place val) (+ place val)))
(define-modify-macro decmodf (&optional (val 1)) (lambda (place val) (- place val)))
(define-modify-macro mulmodf (&optional (val 1)) (lambda (place val) (* place val)))
(define-modify-macro divmodf (&optional (val 1)) (lambda (place val) (/ place val)))

(declaim (ftype (function (fixnum) (simple-array fixnum 1)) make-mod-table)
         (inline make-mod-fact-table))
(defun make-mod-fact-table (size)
  (declare (fixnum size))
  (let ((table (make-array (1+ size)
                           :element-type 'fixnum)))
    (declare ((simple-array fixnum 1) table))
    (setf (aref table 0) 1)
    (loop for i of-type fixnum below size
          do (setf (aref table (1+ i))
                   (* (aref table i)
                      (the fixnum (1+ i)))))
    table))

(declaim (ftype (function (fixnum fixnum (simple-array fixnum 1)) fixnum) mod-combi-with-table)
         (inline mod-combi-with-table))
(defun mod-combi-with-table (n k table)
  (declare (fixnum n k)
           ((simple-array fixnum 1) table))
  (the fixnum
       (if (or (< n k)
               (< n 0)
               (< k 0))
           0
           (* (aref table n)
              (mod-inv (aref table k))
              (mod-inv (aref table (the fixnum (- n k))))))))


;;;
;;; End of inserted contents
;;;
