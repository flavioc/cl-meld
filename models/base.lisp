(in-package :cl-meld)

(defparameter *base-tuples* nil)

(defun base-tuple-defined-p (name)
   (find-if #'(lambda (d) (string-equal (definition-name d) name)) *base-tuples*))

(defmacro deftuple (name types &rest options)
   `(unless (base-tuple-defined-p ,(symbol-name name))
      (push-end (make-definition ,(string-downcase (symbol-name name)) ',types ',options) *base-tuples*)))
      
(defun add-base-tuples ()
   (let ((copy (mapcar #'copy-tree *base-tuples*)))
      (setf *definitions* (append copy *definitions*))))