
(defun tagged-p (list tag)
   (and (listp list)
        (eq (first list) tag)))
        
(defun tagged-tag (list) (first list))

(defun one-elem-p (list) (null (cdr list)))

(defmacro any (predicates val)
   `(or ,@(mapcar (lambda (pred) `(,pred ,val)) predicates)))