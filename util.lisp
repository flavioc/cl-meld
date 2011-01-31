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

(defmacro any (predicates val)
   `(or ,@(mapcar (lambda (pred) `(,pred ,val)) predicates)))
   
(defun dunion (l1 l2) (union l1 l2 :test #'equal))