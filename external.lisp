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

(defmacro define-external-function (name)
   `(progn
      (setf (gethash ,name *external-functions*) *external-functions-counter*)
      (incf *external-functions-counter*)))

(define-external-function "sigmoid")
(define-external-function "randint")
(define-external-function "normalize")
(define-external-function "damp")
(define-external-function "divide")
(define-external-function "convolve")
(define-external-function "addfloatlists")
(define-external-function "intlistlength")
(define-external-function "intlistdiff")
(define-external-function "intlistnth")
(define-external-function "concatenate")