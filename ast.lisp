(in-package :cl-meld)

(defun make-ast (defs clauses axioms nodes)
   `(:ast ,defs ,clauses ,axioms ,nodes))
            
(defun all-definitions (&optional (code *ast*))
   (second code))

(defun definitions (&optional (code *ast*))
   (filter #'definition-p (all-definitions code)))

(defun set-definitions (new-defs)
   (setf (second *ast*) new-defs))
   
(defsetf definitions set-definitions)
(defsetf all-definitions set-definitions)

(defun externs (&optional (code *ast*))
   (filter #'extern-p (all-definitions code)))
  
(defun clauses (&optional (code *ast*))
   (third code))
   
(defun set-clauses (new-clauses)
   (setf (third *ast*) new-clauses))
(defsetf clauses set-clauses)

(defun defined-nodes (&optional (code *ast*))
   (fifth code))

(defun set-defined-nodes (new-nodes)
   (setf (fifth *ast*) new-nodes))
   
(defsetf defined-nodes set-defined-nodes)

(defun axioms (&optional (code *ast*))
   (fourth code))
   
(defun set-axioms (new-axioms)
   (setf (fourth *ast*) new-axioms))
   
(defsetf axioms set-axioms)

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
(defun clause-get-remote-dest (clause)
   (first (clause-get-tagged-option clause :route)))
(defun clause-is-remote-p (clause) (clause-has-tagged-option-p clause :route))
(defun clause-has-delete-p (clause) (clause-has-tagged-option-p clause :delete))
(defun clause-get-delete (clause) (clause-get-tagged-option clause :delete))

(defun is-axiom-p (clause)
   (null (find-if #'subgoal-p (clause-body clause))))