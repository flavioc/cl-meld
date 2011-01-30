
(defun tagged-p (list tag)
   (and (listp list)
        (eq (first list) tag)))
        
(defun tagged-tag (list) (first list))

(defun try-one (ls)
   (if (one-elem-p ls)
      (first ls)
      ls))

(defun one-elem-p (list) (null (cdr list)))

(defun has-elem-p (list el)
   (if (member el list)
      t
      nil))

(defmacro any (predicates val)
   `(or ,@(mapcar (lambda (pred) `(,pred ,val)) predicates)))