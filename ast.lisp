(in-package :cl-meld)

(defun make-ast (defs clauses &optional nodes)
   `(:definitions ,defs :clauses ,clauses :nodes ,nodes))
            
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
   (fourth code))
   
(defun set-clauses (new-clauses)
   (setf (fourth *ast*) new-clauses))
(defsetf clauses set-clauses)

(defun defined-nodes (&optional (code *ast*))
   (sixth code))

(defun set-defined-nodes (new-nodes)
   (setf (sixth *ast*) new-nodes))
   
(defsetf defined-nodes set-defined-nodes)