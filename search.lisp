
(in-package :cl-meld)
   
(defun get-assignments (body) (filter #'assignment-p body))
(defun get-assignment-vars (assignments) (mapcar #'assignment-var assignments))
(defun get-subgoals (code) (filter #'subgoal-p code))
(defun get-non-subgoals (code) (remove-if #'subgoal-p code))
(defun get-comprehensions (code) (filter #'comprehension-p code))
(defun get-agg-constructs (code) (filter #'agg-construct-p code))
(defun get-constraints (code) (remove-if-not #'constraint-p code))
(defun recursively-get-subgoals (code)
	(append (get-subgoals code)
		(do-conditionals code (:term1 term1 :term2 term2 :operation append)
			(append (recursively-get-subgoals term1)
						(recursively-get-subgoals term2)))))

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
                     ((clause-p expr)
                        (aux (clause-head expr))
                        (aux (clause-body expr)))
                     ((subgoal-p expr)
								(dolist (arg (subgoal-args expr))
									(aux arg))
								(let ((dest (subgoal-get-remote-dest expr)))
									(when dest
										(aux (make-var dest :type-addr)))))
							((exist-p expr)
								(with-exist expr (:var-list vars :body body)
									(dolist (var vars)
										(aux var))
									(aux body)))
                     ((comprehension-p expr)
                        (with-comprehension expr (:left left :right right)
                           (aux left)
                           (aux right)))
							((conditional-p expr)
								(with-conditional expr (:cmp cmp :term1 term1 :term2 term2)
									(aux cmp)
									(aux term1)
									(aux term2)))
                     ((constraint-p expr) (aux (constraint-expr expr)))
                     ((assignment-p expr)
                        (aux (assignment-var expr))
                        (aux (assignment-expr expr)))
							((agg-construct-p expr)
								(aux (agg-construct-body expr))
								(aux (agg-construct-head expr)))
                     ((if-p expr)
                        (aux (if-cmp expr))
                        (aux (if-e1 expr))
                        (aux (if-e2 expr)))
							((bool-p expr) nil)
                     ((var-p expr) nil)
                     ((int-p expr) nil)
                     ((float-p expr) nil)
                     ((host-id-p expr) nil)
                     ((nil-p expr) nil)
                     ((world-p expr) nil)
                     ((cpus-p expr) nil)
                     ((addr-p expr) nil)
                     ((argument-p expr) nil)
							((get-constant-p expr) nil)
							((callf-p expr) (dolist (arg (callf-args expr)) (aux arg)))
							((call-p expr) (dolist (arg (call-args expr)) (aux arg)))
							((struct-p expr)
								(loop for subexpr in (struct-list expr)
									do (aux subexpr)))
							((struct-val-p expr)
								(aux (struct-val-var expr)))
                     ((cons-p expr)
                        (aux (cons-head expr))
                        (aux (cons-tail expr)))
                     ((head-p expr) (aux (head-list expr)))
                     ((tail-p expr) (aux (tail-list expr)))
                     ((not-p expr) (aux (not-expr expr)))
                     ((test-nil-p expr) (aux (test-nil-expr expr)))
                     ((convert-float-p expr) (aux (convert-float-expr expr)))
                     ((let-p expr)
								(aux (let-var expr))
								(aux (let-expr expr))
								(aux (let-body expr)))
                     ((op-p expr)
                        (aux (op-op1 expr))
                        (aux (op-op2 expr)))
                     ((and (listp expr)
                           (not (symbolp (first expr)))
                           (listp (first expr)))
                        (dolist (el expr)
                           (aux el)))
							((null expr) )
                     (t (error 'expr-invalid-error :text (tostring "iterate-expr: Invalid expression: ~a" expr))))))
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


(defmacro find-constraints (body &rest fns)
	`(filter #L(and (constraint-p !1) ,@(loop for fn in fns collect `(funcall ,fn (constraint-expr !1)))) body))
	
(defmacro find-constraints-expr (body &rest fns)
	`(mapcar #'constraint-expr (find-constraints ,body ,@fns)))

(defun constraint-by-var1 (var-name expr) (var-eq-p var-name (op-op1 expr)))
(defun constraint-by-var2 (var-name expr) (var-eq-p var-name (op-op2 expr)))

(defun subgoal-by-name (name) #L(string-equal name (subgoal-name !1)))

(defun find-assignment-constraints-expr (body var)
	"Finds all assignments in body and returns their right hand side expression."
   (find-constraints-expr body
                     #L(equal-p !1)
                     #L(constraint-by-var1 var !1)))

(defun find-first-assignment-constraint-to-var (body var)
	"Finds the first constraint assignment of the form var = var2."
	(let ((ret (find-constraints body
							#L(equal-p !1)
							#L(constraint-by-var1 var !1)
							#L(var-p (op-op2 !1)))))
		(if ret
			(values (first ret) (op-op2 (constraint-expr (first ret))))
			(let ((ret2 (find-constraints body
								#L(equal-p !1)
								#L(constraint-by-var2 var !1)
								#L(var-p (op-op1 !1)))))
				(when ret2
					(values (first ret2) (op-op1 (constraint-expr (first ret)))))))))

(defun find-not-constraints (body)
	(find-constraints body #L(not-p !1)))
	
(defun find-test-nil-constraints (body)
	(find-constraints body #L(test-nil-p !1)))

(defun find-assignment-constraints (body var)
	(find-constraints body #L(equal-p !1) #L(constraint-by-var1 var !1)))

(defun find-assignment-by-var (body var)
	"Finds the first assignment expression of a certain variable."
	(let ((r (find-if #'(lambda (a) (and (assignment-p a) (var-eq-p var (assignment-var a)))) body)))
		(when r
			(assignment-expr r))))

(defun subgoal-appears-code-p (code subname)
	(find-if #L(string-equal subname (subgoal-name !1)) (get-subgoals code)))
	
(defun clause-body-matches-subgoal-p (clause subgoal-name)
   (subgoal-appears-code-p (clause-body clause) subgoal-name))
(defun clause-head-matches-subgoal-p (clause subgoal-name)
   (subgoal-appears-code-p (clause-head clause) subgoal-name))
(defun clause-matches-subgoal-p (clause subgoal-name)
   (or (clause-body-matches-subgoal-p clause subgoal-name)
       (clause-head-matches-subgoal-p clause subgoal-name)))
   
(defun subgoal-number-of-occurrences (code subgoal-name)
   (letret (total 0)
      (do-subgoals code (:name name)
         (when (string-equal name subgoal-name)
            (incf total)))))
(defun subgoal-has-var-p (subgoal var)
	(with-subgoal subgoal (:args args)
		(dolist (arg args)
			(when (var-eq-p arg var)
				(return-from subgoal-has-var-p t))))
	nil)
	
(defun subgoals-in-list-have-var-p (ls var)
	(do-subgoals ls (:args args)
		(dolist (arg args)
			(when (var-eq-p arg var)
				(return-from subgoals-in-list-have-var-p t))))
	nil)		
				
(defun clause-body-number-of-occurrences (clause subgoal-name)
   (subgoal-number-of-occurrences (clause-body clause) subgoal-name))
(defun clause-head-number-of-occurrences (clause subgoal-name)
   (subgoal-number-of-occurrences (clause-head clause) subgoal-name))
(defun clause-number-of-occurrences (clause subgoal-name)
   (+ (clause-body-number-of-occurrences clause subgoal-name)
      (clause-head-number-of-occurrences clause subgoal-name)))

(defun is-fact-p (pred-name)
   "Given a predicate name tells you if it is a fact in the program."
   (do-rules (:clause clause)
      (when (clause-head-matches-subgoal-p clause pred-name)
         (return-from is-fact-p nil)))
   t)

(defun find-clauses-with-subgoal-in-body (subgoal-name)
   (filter #'(lambda (clause) (clause-body-matches-subgoal-p clause subgoal-name))
				*clauses*))
				
(defun get-variable-equal-constraints (constraints)
	(filter #'(lambda (c) (let ((e (constraint-expr c)))
									(and (eq (op-op e) :equal)
												(var-p (op-op1 e)))))
	 							constraints))
	
(defun expr-is-constant-p (expr constraints assigns)
	(expr-is-constant-aux-p expr constraints assigns))

(defun expr-is-constant-aux-p (expr constraints assigns)
	"Decides if the 'expr' is a constant by using the equal operations in constraints and the assignments.
	Returns a new constant and computable expression."
	(cond
		((bool-p expr) expr)
		((host-id-p expr) nil)
		((literal-p expr) expr)
		((nil-p expr) expr)
		((call-p expr) nil)
		((callf-p expr) nil)
		((cons-p expr) nil)
		((world-p expr) nil)
      ((cpus-p expr) nil)
		((struct-p expr) nil)
		((struct-val-p expr) nil)
		((if-p expr) 
			(let ((c (expr-is-constant-aux-p (if-cmp expr) constraints assigns)))
				(when c
					(let ((e1 (expr-is-constant-aux-p (if-e1 expr) constraints assigns)))
						(when e1
							(let ((e2 (expr-is-constant-aux-p (if-e2 expr) constraints assigns)))
								(make-if c e1 e2 (expr-type expr))))))))
		((convert-float-p expr)
			(let ((i (expr-is-constant-aux-p (convert-float-expr expr) constraints assigns)))
				(when i
					(make-convert-float i))))
		((test-nil-p expr)
			(let ((x (expr-is-constant-aux-p (test-nil-expr expr) constraints assigns)))
				(cond
					((and x (cons-p x))
						(make-bool t))
					((and x (nil-p x))
						(make-bool nil))
					(t nil))))
		((head-p expr)
			(let ((c (expr-is-constant-aux-p (head-list expr) constraints assigns)))
				(when c
					(make-head c (expr-type expr)))))
		((not-p expr)
			(let ((c (expr-is-constant-aux-p (not-expr expr) constraints assigns)))
				(when c
					(make-not c (expr-type expr)))))
		((get-constant-p expr)
			(let ((c (lookup-const (get-constant-name expr))))
				(expr-is-constant-aux-p (const-definition-expr c) nil nil)))
		((var-p expr)
			(let ((cs (find-assignment-constraints-expr constraints expr)))
				(dolist (c cs)
					(let ((ret (expr-is-constant-aux-p (op-op2 c) constraints assigns)))
						(when ret
							(return-from expr-is-constant-aux-p ret))))
				(let ((ass-expr (find-assignment-by-var assigns expr)))
					(when ass-expr
						(expr-is-constant-aux-p ass-expr constraints assigns)))))
		((op-p expr)
			(let ((e1 (op-op1 expr))
					(e2 (op-op2 expr)))
				(let ((c1 (expr-is-constant-aux-p e1 constraints assigns)))
					(when c1
						(let ((c2 (expr-is-constant-aux-p e2 constraints assigns)))
							(when c2
								(make-op (op-op expr) c1 c2)))))))
		((tail-p expr)
			(let ((ret (expr-is-constant-aux-p (tail-list expr) constraints assigns)))
				(when ret
					(make-tail ret))))
		(t
			(warn "-----NOT CONSIDERING ~a as constant-----" expr)
			nil)))
			
(defmacro compute-arith-expr (expr c1 c2 op)
	(alexandria:with-gensyms (typ)
		`(let ((,typ (expr-type ,expr)))
			(cond
				((and (int-p ,c1) (int-p ,c2))
					(make-int (,op (int-val ,c1) (int-val ,c2)) :type-int))
				((and (int-p ,c1) (float-p ,c2))
					(if (eq ,typ :type-int)
						(make-int (,op (int-val ,c1) (float-val ,c2)) ,typ)
						(make-float (,op (int-val ,c1) (float-val ,c2)))))
				((and (float-p ,c1) (int-p ,c2))
					(if (eq ,typ :type-int)
						(make-int (,op (float-val ,c1) (int-val ,c2)) ,typ)
						(make-float (,op (float-val ,c1) (int-val ,c2)))))
				((and (float-p ,c1) (float-p ,c2))
					(make-float (,op (float-val ,c1) (float-val ,c2))))
				(t (error 'expr-invalid-error :text (tostring "cannot optimize arithmetic operation ~a" ,expr)))))))
			
(defun compute-constant-expr (expr)
	"Compiles constant expression into a constant."
	(cond
		((const-p expr) expr)
		((bool-p expr) expr)
		((nil-p expr) expr)
		((op-p expr)
			(let* ((op (op-op expr))
				    (e1 (op-op1 expr))
				 	 (e2 (op-op2 expr))
					 (c1 (compute-constant-expr e1))
					 (c2 (compute-constant-expr e2)))
				(case op
					(:plus (compute-arith-expr expr c1 c2 +))
					(:minus (compute-arith-expr expr c1 c2 -))
					(:mul (compute-arith-expr expr c1 c2 *))
					(:div (compute-arith-expr expr c1 c2 /))
					(:or (let* ((e1 (op-op1 expr))
								(e2 (op-op2 expr))
								(c1 (compute-constant-expr e1))
								(c2 (compute-constant-expr c2)))
							(make-bool (or (bool-val c1) (bool-val c2)))))
					(:lesser (make-bool (< (int-float-val c1) (int-float-val c2))))
					(:lesser-equal (make-bool (<= (int-float-val c1) (int-float-val c2))))
					(:equal (cond
								((and (int-p c1) (int-p c2))
									(make-bool (= (int-val c1) (int-val c2))))
								((and (float-p c1) (float-p c2))
									(make-bool (= (float-val c1) (float-val c2))))
								(t (error 'expr-invalid-error :text (tostring "evaluation of ~a is not supported yet." expr)))))
					(t (error 'expr-invalid-error :text (tostring "evaluation of ~a is not supported yet." expr))))))
		((tail-p expr)
			(let ((res (compute-constant-expr (tail-list expr))))
				(cons-tail res)))
		(t
			(error 'expr-invalid-error :text (tostring "evaluation of ~a is not supported yet." expr)))))
			
