
(in-package :cl-meld)

(defparameter *var-counter* 0)
(defun generate-random-var ()
   "Generates a new variable name."
   (make-var (tostring "MV~a" (incf *var-counter*))))

(defun iterate-expr (fn expr)
   (unless expr
      (return-from iterate-expr nil))
   (let ((ls (list)))
      (labels ((aux (expr)
                  (let ((val (funcall fn expr)))
                     (cond
                        ((eq val :stop) (return-from aux))
                        (val
                           (push val ls))))
                  (cond
                     ((subgoal-p expr) (dolist (arg (subgoal-args expr)) (aux arg)))
                     ((constraint-p expr) (aux (constraint-expr expr)))
                     ((assignment-p expr)
                        (aux (assignment-var expr))
                        (aux (assignment-expr expr)))
                     ((var-p expr) nil)
                     ((int-p expr) nil)
                     ((float-p expr) nil)
                     ((host-id-p expr) nil)
                     ((nil-p expr) nil)
                     ((world-p expr) nil)
                     ((addr-p expr) nil)
                     ((call-p expr) (dolist (arg (call-args expr)) (aux arg)))
                     ((cons-p expr)
                        (aux (cons-head expr))
                        (aux (cons-tail expr)))
                     ((head-p expr) (aux (head-list expr)))
                     ((tail-p expr) (aux (tail-list expr)))
                     ((not-p expr) (aux (not-expr expr)))
                     ((test-nil-p expr) (aux (test-nil-expr expr)))
                     ((convert-float-p expr) (aux (convert-float-expr expr)))
                     ((colocated-p expr)
                        (aux (colocated-first expr))
                        (aux (colocated-second expr)))
                     ((op-p expr)
                        (aux (op-op1 expr))
                        (aux (op-op2 expr)))
                     ((and (listp expr)
                           (not (symbolp (first expr)))
                           (listp (first expr)))
                        (dolist (el expr)
                           (aux el)))
                     (t (error 'expr-invalid-error :text (tostring "Invalid expression: ~a" expr))))))
            (aux expr)
            ls)))
      
(defun all-variables (expr)
   (let ((vars (iterate-expr #'(lambda (x)
                                 (cond
                                    ((var-p x) x))) expr)))
      (remove-duplicates vars :test #'equal)))
      
(defun all-variable-names (expr) (mapcar #'var-name (all-variables expr)))

(defun valid-assignment-p (vars) #'(lambda (a) (tree-subsetp (all-variable-names (assignment-expr a)) vars)))
(defun select-valid-assignments (body subgoals &optional (base-vars nil))
   (loop with vars = (union base-vars (all-variable-names subgoals))
         with ass = (get-assignments body)
         with ret = nil
         for (next-assignments . new-ass) = (split (valid-assignment-p vars) ass)
         while next-assignments
         do (setf ass new-ass)
         do (push-all next-assignments ret)
         do (push-all (mapcar #L(var-name (assignment-var !1)) next-assignments) vars)
         finally (return ret)))

(defun unneeded-assignment-p (body)
   #'(lambda (a)
         (let ((var-name (var-name (assignment-var a)))
               (vars (all-variable-names (remove-tree a body))))
            (not (has-elem-p vars var-name)))))

(defun remove-unneeded-assignments (body &optional head)
   (loop with ass = (get-assignments body)
         for (next-unneeded . next-ass) = (split (unneeded-assignment-p (append body head)) ass)
         while next-unneeded
         do (setf ass next-ass
                  body (remove-all body next-unneeded))
         finally (return body)))
         
(defun is-fact-p (code pred-name)
   "Given a predicate name tells you if it is a fact in the program."
   (do-clauses (clauses code) (:body body)
      (if (some #'(lambda (sub) (equal (subgoal-name sub) pred-name)) (get-subgoals body))
         (return-from is-fact-p t)))
   nil)

(defun find-constraints (body fn)
   (filter #L(and (constraint-p !1) (funcall fn (constraint-expr !1))) body))
   
(defun constraint-by-var1 (var-name expr) (var-eq-p var-name (op-op1 expr)))
(defun constraint-by-var2 (var-name expr) (var-eq-p var-name (op-op2 expr)))
