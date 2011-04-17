(in-package :cl-meld)

(defun create-bin-array (&optional (size 0)) (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun str->sym (str) (values (intern str)))

(defun tagged-p (list tag)
   (and (listp list)
        (eq (first list) tag)))        
(defun tagged-tag (list) (first list))

(defun one-elem-p (list)
   (if (listp list)
      (null (cdr list))
      t))

(defun try-one (ls)
   (if (one-elem-p ls)
      (first ls)
      ls))

(defun has-elem-p (list el) (ensure-bool (member el list)))
(defun has-test-elem-p (list el test) (ensure-bool (member el list :test test)))

(defun create-hash-set (ls)
   (let ((hash (make-hash-table :test #'equal)))
      (dolist (a ls)
         (setf (gethash a hash) t))
      hash))
      
(defun remove-hash-set (hash-set el) (remhash el hash-set))
(defun in-hash-set-p (hash-set el) (gethash el hash-set))

(defmacro do-hash-set ((el hash-set) &body body)
   `(loop for ,el being the hash-keys of ,hash-set
         do (progn ,@body)))

(defmacro any (predicates val)
   `(or ,@(mapcar (lambda (pred) `(,pred ,val)) predicates)))
   
(defun dunion (l1 l2) (union l1 l2 :test #'equal))

(defmacro filter (&rest args) `(remove-if-not ,@args))

(defun mapfilter (trans f l) (mapcar trans (filter f l)))

(defun enumerate (a b)
   (if (> a b)
      nil
      (cons a (enumerate (1+ a) b))))

(defun remove-tree (tree ls) (remove tree ls :test #'equal))
(defun delete-all (from ls) (dolist (el ls) (setf from (delete el from :test #'equal))) from)
(defun remove-all (from ls) (reduce #L(remove-tree !2 !1) ls :initial-value from))

(defmacro push-all (ls to)
   (with-gensyms (el)
      `(dolist (,el ,ls)
         (push ,el ,to))))
         
(defmacro push-dunion (el to) `(setf ,to (dunion (list ,el) ,to)))

(defmacro push-dunion-all (ls to) `(setf ,to (dunion ,ls ,to)))

(defmacro set-tree-difference (t1 t2) `(set-difference ,t1 ,t2 :test #'equal))
(defmacro tree-intersection (t1 t2) `(intersection ,t1 ,t2 :test #'equal))
(defmacro tree-subsetp (t1 t2) `(subsetp ,t1 ,t2 :test #'equal))

(defun split (fn l)
   (let ((l1 (filter fn l)))
      (if l1
         (cons l1 (remove-if fn l))
         (cons nil l))))
(defun split-mult-return (fn l)
   (destructuring-bind (filtered . removed) (split fn l)
      (values filtered removed)))
         
(defmacro push-end (el ls)
   `(setf ,ls (append ,ls (list ,el))))
   
(defmacro format-keyword (control &rest arguments)
   `(format-symbol "KEYWORD" ,control ,@arguments))
   
(defun addify (ls &optional (n 0))
   (if (null ls)
      nil
      (cons n (addify (rest ls) (+ n (first ls))))))