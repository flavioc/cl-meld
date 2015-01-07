(in-package :cl-meld)

(defparameter *major-version* 0)
(defparameter *minor-version* 11)

(defparameter *init-tuple* (make-definition "_init" '(:type-addr) '(:init-tuple :linear)))

(defparameter *base-tuples* nil)

(defun base-tuple-defined-p (name)
   (find-if #'(lambda (d) (string-equal (definition-name d) name)) *base-tuples*))

(defmacro deftuple (name types &rest options)
   (let ((real-name (if (symbolp name) (string-downcase (symbol-name name)) name)))
      `(unless (base-tuple-defined-p ,real-name)
         (push-end (make-definition ,real-name ',types ',options) *base-tuples*))))

(defun ast-add-base-tuples (ast use-threads-p)
   (let ((copy (mapcar #'copy-tree *base-tuples*)))
      (when use-threads-p
         (push (make-definition "_init_thread" '(:type-thread) '(:init-tuple :linear :thread)) copy))
      (push (copy-tree *init-tuple*) copy)
      (setf (definitions ast) (append copy (definitions ast)))))
