(in-package :cl-meld)

(define-condition external-invalid-error (error)
   ((text :initarg :text :reader text)))

(defparameter *external-functions* (make-hash-table :test #'equal))
(defparameter *external-functions-counter* 0)

(defun lookup-external-function-id (name)
   (multiple-value-bind (id found-p) (gethash name *external-functions*)
      (unless found-p
         (error 'external-invalid-error :text (tostring "invalid external function: ~a" name)))
      id))
      
(defmacro define-external-functions (&rest names)
   `(on-top-level
      ,@(mapcar #'(lambda (name)
                     (setf (gethash name *external-functions*) *external-functions-counter*)
                     (incf *external-functions-counter*))
               names)))

(define-external-functions "sigmoid")