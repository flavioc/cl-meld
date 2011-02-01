(in-package :cl-meld)

(defun get-homes (subgoals)
   (with-var homes
      (do-subgoals subgoals (name args)
         (declare (ignore name))
         (setf homes (union (list (var-name (first args))) homes)))))

(defun get-routes (code)
   (mapfilter #'definition-name #L(has-elem-p (definition-options !1) :route) (definitions code)))
   
(defun make-route (v1 v2) `(:from ,(var-name v2) :to ,(var-name v1)))
            
(defun get-paths (subgoals routes)
   (with-var paths
      (do-subgoals subgoals (name args)
         (when (has-test-elem-p routes name #'string-equal) ; this is a route
            (push (make-route (first args) (second args)) paths)))))

(defun localize (code)
   (let ((routes (get-routes code)))
      (do-clauses code (head body)
         (let ((homes (get-homes body)))
            (format t "homes: ~A~%" homes)
            (let ((paths (get-paths body routes)))
               (format t "paths: ~A~%" paths))))
      (format t "routes: ~A~%" routes))
   code)