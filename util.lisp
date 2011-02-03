(in-package :cl-meld)

(defun tagged-p (list tag)
   (and (listp list)
        (eq (first list) tag)))        
(defun tagged-tag (list) (first list))

(defun one-elem-p (list) (null (cdr list)))

(defun try-one (ls)
   (if (one-elem-p ls)
      (first ls)
      ls))

(defun has-elem-p (list el) (ensure-bool (member el list)))
(defun has-test-elem-p (list el test) (ensure-bool (member el list :test test)))

(defmacro any (predicates val)
   `(or ,@(mapcar (lambda (pred) `(,pred ,val)) predicates)))
   
(defun dunion (l1 l2) (union l1 l2 :test #'equal))

(defmacro filter (&rest args) `(remove-if-not ,@args))

(defun mapfilter (trans f l) (mapcar trans (filter f l)))

(defun enumerate (a b)
   (if (> a b)
      nil
      (cons a (enumerate (1+ a) b))))
      
(defun delete-all (from ls) (dolist (el ls) (setf from (delete el from :test #'equal))) from)
(defun remove-all (from ls) (reduce #L(remove !2 !1 :test #'equal) ls :initial-value from))

(defmacro push-all (ls to)
   (with-gensyms (el)
      `(dolist (,el ,ls)
         (push ,el ,to))))