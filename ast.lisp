(in-package :cl-meld)

(defclass ast ()
   ((definitions
      :initarg :definitions
      :initform (error "missing definitions.")
      :accessor definitions)
    (externs
      :initarg :externs
      :initform (error "missing externs.")
      :accessor externs)
    (clauses
      :initarg :clauses
      :initform (error "missing clauses.")
      :accessor clauses)
    (axioms
      :initarg :axioms
      :initform (error "missing axioms")
      :accessor axioms)
    (nodes
      :initarg :nodes
      :initform (error "missing nodes")
      :accessor nodes)))
      
(defun make-ast (defs externs clauses axioms nodes)
   (make-instance 'ast
      :definitions defs
      :externs externs
      :clauses clauses
      :axioms axioms
      :nodes nodes))

;;;;;;;;;;;;;;;;;;;
;; Clauses
;;;;;;;;;;;;;;;;;;;

(defun make-clause (perm conc &rest options) `(:clause ,perm ,conc ,options))
(defun clause-p (clause) (tagged-p clause :clause))
(defun clause-head (clause) (third clause))
(defun clause-body (clause) (second clause))
(defun set-clause-body (clause new-body)
   (setf (second clause) new-body))
(defsetf clause-body set-clause-body)

(defun clause-options (clause) (fourth clause))
(defun clause-add-option (clause opt) (push opt (fourth clause))) 
(defun clause-has-tagged-option-p (clause opt) (option-has-tag-p (clause-options clause) opt))
(defun clause-get-tagged-option (clause opt)
   (let ((res (find-if #L(tagged-p !1 opt) (clause-options clause))))
      (when res
         (rest res))))
(defun clause-get-all-tagged-options (clause opt)
   (mapfilter #'rest #L(tagged-p !1 opt) (clause-options clause)))
(defun clause-add-tagged-option (clause opt &rest rest)
   (clause-add-option clause `(,opt ,@rest)))
(defun clause-get-remote-dest (clause)
   (first (clause-get-tagged-option clause :route)))
(defun clause-is-remote-p (clause) (clause-has-tagged-option-p clause :route))
(defun clause-has-delete-p (clause) (clause-has-tagged-option-p clause :delete))
(defun clause-get-all-deletes (clause)
   (clause-get-all-tagged-options clause :delete))
(defun clause-get-delete (clause name)
   (find-if #L(string-equal (first !1) name) (clause-get-all-deletes clause)))
(defun clause-add-delete (clause name args)
   (clause-add-tagged-option clause :delete name args))
   
(defun delete-option-args (delete-opt) (second delete-opt))
(defun delete-option-name (delete-opt) (first delete-opt))

(defun is-axiom-p (clause)
   (null (find-if #'subgoal-p (clause-body clause))))
   