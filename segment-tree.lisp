;;;
;;; BOF
;;;

;; Segment-tree (1-indexed)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern
     (with-output-to-string (s)
       (dolist (x args)
         (princ (string-upcase x) s))))))

(defmacro define-segment-tree (struct-name &key element-type result-type fn e)
  (let* ((name-str (symbol-name struct-name))
         (conc-name (symb name-str "-"))
         (constructor (symb "%make-" name-str))
         (make-seg (symb "make-" name-str))
         (seg-ref (symb name-str "-ref"))
         (seg-fold (symb name-str "-fold"))
         (seg-update (symb name-str "-update")))
    `(progn

       (defstruct (,struct-name (:conc-name ,conc-name)
                                (:constructor ,constructor))
         (m nil :type fixnum)
         (data nil :type (simple-array ,element-type (*))))

       (declaim (inline ,make-seg
                        ,seg-fold
                        ,seg-update))
       (defun ,make-seg (size)
         (declare (fixnum size))
         (let ((m (sb-int:named-let rec ((m 1))
                    (if (>= m size)
                        m
                        (rec (ash m 1))))))
           (declare (fixnum m))
           (,constructor :m m
                         :data (make-array (the fixnum (ash m 1))
                                                                         :element-type ',element-type
                                                                         :adjustable nil
                                                                         :initial-element ,e))))

       (defun ,seg-ref (seg idx)
         (declare (,struct-name seg)
                  (fixnum idx))
         (with-slots (data m) seg
           (aref data (the fixnum
                           (+ idx m)))))

       (defun ,seg-fold (seg l r)
         (declare (,struct-name seg)
                  (fixnum l r))
         (with-slots (m data) seg
           (let ((l (+ l m))
                 (r (+ r m)))
             (declare (fixnum l r))
             (loop while (< l r)
                   with res of-type ,result-type = ,e
                   when (logbitp 0 l)
                     do (setf res (,fn (aref data l) res))
                     and do (incf l)
                   when (logbitp 0 r)
                     do (setf res (,fn res (aref data (1- r))))
                     and do (decf r)
                   do (setq l (ash l -1))
                      (setq r (ash r -1))
                   finally
                      (return res)))))


       (defun ,seg-update (seg i val)
         (declare (,struct-name seg)
                  (fixnum i)
                  (,element-type val))
         (with-slots (m data) seg
           (let ((i (the fixnum (+ i m))))
             (declare (fixnum i))
             (setf (aref data i) val)
             (let ((i (ash i -1)))
               (declare (fixnum i))
               (loop while (plusp i)
                     do (setf (aref data i)
                              (the ,result-type
                                   (,fn (aref data (the fixnum (logior 0 (ash i 1))))
                                        (aref data (the fixnum (logior 1 (ash i 1)))))))
                        (setf i (the fixnum (ash i -1)))))))))))

;; e.g. Range-Minimum-Query(RMQ)
(define-segment-tree seg
  :element-type fixnum
  :result-type fixnum
  :fn min
  :e most-positive-fixnum)

;;;
;;; EOF
;;;
