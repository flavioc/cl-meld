(in-package :cl-meld)

(define-condition type-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun check-home-argument (name typs)
   (when (null typs)
      (error 'type-invalid-error :text (concatenate 'string name " has no arguments")))
   (unless (or (type-addr-p (first typs))
               (type-worker-p (first typs)))
      (error 'type-invalid-error
         :text (concatenate 'string "first argument of tuple " name " must be of type 'node' or 'worker'"))))
         
(defun valid-aggregate-p (agg)
   (let ((agg (aggregate-agg agg))
         (typ (aggregate-type agg)))
      (case agg
         (:first t)
         (:sum
            (eq-or typ :type-int :type-float :type-list-int :type-list-float))
         ((:min :max)
            (eq-or typ :type-int :type-float)))))
            
(defun update-aggregate-head (head body modifier edge-name agg-name get-fun)
   ;; If this rule produces an aggregate fact, push the route node into the last
   ;; argument or else put the home node (for local rules)
   (let ((head-subs (filter #L(equal (subgoal-name !1) agg-name) (get-subgoals head))))
      (when head-subs
         (let* ((host (first-host-node head))
                (routes (filter #L(equal (subgoal-name !1) edge-name) (get-subgoals body))))
            (if routes
               (setf host (funcall get-fun (subgoal-args (first routes))))
               (unless (aggregate-mod-includes-home-p modifier)
                  (aggregate-mod-include-home modifier)))
            (loop for sub in head-subs
                  do (push-end host (subgoal-args sub)))))))
         

(defun update-aggregate-input (modifier edge-name agg-name get-fun)
   "For an aggregate that has an INPUT/OUTPUT modifier, executes source code transformations
   that puts the input/output node as the last argument of the aggregate"
   (do-axioms (:head head)
      (update-aggregate-head head nil modifier edge-name agg-name get-fun))
   (do-rules (:head head :body body)
      (update-aggregate-head head body modifier edge-name agg-name get-fun)
      ;; Add an unnamed variable for clauses that use the aggregated result.
      (let ((body-subs (filter #L(equal (subgoal-name !1) agg-name) (get-subgoals body))))
         (loop for sub in body-subs
               do (push-end (generate-random-var) (subgoal-args sub)))))
   (let ((def (lookup-definition agg-name)))
      (assert (not (null def)))
      (push-end :type-addr (definition-types def))))

(defun valid-aggregate-modifier-p (agg-name agg)
   (let ((aggmod (aggregate-mod agg)))
      (cond
         ((null aggmod) t)
         ((aggregate-mod-is-immediate-p aggmod) t)
         ((aggregate-mod-is-input-p aggmod)
            (let* ((name (aggregate-mod-io-name aggmod))
                   (def (lookup-definition name))
                   (ret (and def (is-route-p def))))
               (when ret
                  (update-aggregate-input aggmod name agg-name #'first))
               ret))
         ((aggregate-mod-is-output-p aggmod)
            (let* ((name (aggregate-mod-io-name aggmod))
                   (def (lookup-definition name))
                   (ret (and def (is-route-p def))))
               (when ret
                  (update-aggregate-input aggmod name agg-name #'second))
               ret)))))

(defun check-aggregates (name typs)
   (let ((total (count-if #'aggregate-p typs)))
      (unless (<= total 1)
         (error 'type-invalid-error
            :text (concatenate 'string "tuple " name " must have only one aggregate")))
      (when-let ((agg (find-if #'aggregate-p typs)))
         (unless (valid-aggregate-p agg)
            (error 'type-invalid-error
               :text "invalid aggregate type"))
         (unless (valid-aggregate-modifier-p name agg)
            (error 'type-invalid-error
               :text "invalid aggregate modifier")))))
         
(defun no-types-p (ls) (null ls))
(defun merge-types (ls types) (intersection ls types))
(defun valid-type-combination-p (types)
   (equal-or types (:type-int) (:type-float) (:type-int :type-float) (:type-bool) (:type-addr) (:type-worker)
                   (:type-list-int) (:type-list-float) (:type-list-addr)))
   
(defparameter *constraints* nil)
(defparameter *defined* nil)
(defparameter *defined-in-context* nil)

(defmacro with-typecheck-context (&body body)
   `(let ((*defined* nil)
          (*defined-in-context* nil)
          (*constraints* (make-hash-table)))
      ,@body))

(defmacro extend-typecheck-context (&body body)
   `(let ((*defined* (copy-list *defined*))
          (*defined-in-context* nil)
          (*constaints* (copy-hash-table *constraints*)))
      ,@body))

(defun variable-is-defined (var)
   (unless (has-elem-p *defined* (var-name var))
      (push (var-name var) *defined-in-context*)
      (push (var-name var) *defined*)))
(defun variable-defined-p (var) (has-elem-p *defined* (var-name var)))
(defun has-variables-defined (expr) (every #'variable-defined-p (all-variables expr)))

(defun set-type (expr typs)
   (let ((typ (list (try-one typs))))
      (cond
         ((or (nil-p expr) (world-p expr)) (setf (cdr expr) typ))
         ((or (var-p expr) (int-p expr) (float-p expr) (tail-p expr) (head-p expr)
               (not-p expr) (test-nil-p expr) (addr-p expr) (convert-float-p expr))
            (setf (cddr expr) typ))
         ((or (call-p expr) (op-p expr) (cons-p expr) (colocated-p expr)) (setf (cdddr expr) typ))
         (t (error 'type-invalid-error :text (tostring "Unknown expression ~a to set-type" expr))))))
      
(defun force-constraint (var new-types)
   (multiple-value-bind (types ok) (gethash var *constraints*)
      (when ok
         (setf new-types (merge-types types new-types))
         (when (no-types-p new-types)
            (error 'type-invalid-error :text
                  (tostring "Type error in variable ~a: new constraint are types ~a but variable is set as ~a" var new-types types))))
      (setf (gethash var *constraints*) new-types)))
      
(defun select-simpler-types (types)
   (cond
      ((null (set-difference types *number-types*))
       (intersection types '(:type-int)))
      ((null (set-difference  types *list-number-types*))
       (intersection types '(:type-list-int)))))

(defun list-base-type (typ)
   (case typ
      (:type-list-int :type-int)
      (:type-list-float :type-float)
      (:type-list-addr :type-addr)))
(defun list-type (typ)
   (case typ
      (:type-int :type-list-int)
      (:type-float :type-list-float)
      (:type-addr :type-list-addr)))
         
(defun get-type (expr forced-types)
   (labels ((do-get-type (expr forced-types)
            (cond
               ((var-p expr) (force-constraint (var-name expr) forced-types))
               ((int-p expr) (merge-types forced-types '(:type-int :type-float)))
               ((float-p expr) (merge-types forced-types '(:type-float)))
               ((addr-p expr) (merge-types forced-types '(:type-addr)))
               ((call-p expr)
                  (let ((extern (lookup-extern (call-name expr))))
                     (unless extern (error 'type-invalid-error :text (tostring "undefined call ~a" (call-name expr))))
                     (loop for typ in (extern-types extern)
                           for arg in (call-args expr)
                           do (get-type arg `(,typ)))
                     (merge-types forced-types `(,(extern-ret-type extern)))))
               ((convert-float-p expr)
                  (get-type (convert-float-expr expr) '(:type-int))
                  (merge-types forced-types '(:type-float)))
               ((nil-p expr) (merge-types forced-types *list-types*))
               ((world-p expr) (merge-types '(:type-int) forced-types))
               ((colocated-p expr)
                  (get-type (colocated-first expr) '(:type-addr))
                  (get-type (colocated-second expr) '(:type-addr))
                  (merge-types forced-types '(:type-bool)))
               ((cons-p expr)
                  (let* ((tail (cons-tail expr))
                         (head (cons-head expr))
                         (base-types (mapcar #'list-base-type forced-types))
                         (head-types (get-type head base-types))
                         (new-types (merge-types (mapcar #'list-type head-types) forced-types)))
                     (get-type tail new-types)))
               ((head-p expr)
                  (let ((ls (head-list expr))
                        (list-types (mapcar #'list-type forced-types)))
                     (mapcar #'list-base-type (get-type ls list-types))))
               ((tail-p expr)
                  (get-type (tail-list expr) forced-types))
               ((not-p expr)
                  (merge-types forced-types (get-type (not-expr expr) '(:type-bool)))) 
               ((test-nil-p expr)
                  (get-type (test-nil-expr expr) *list-types*)
                  (merge-types forced-types '(:type-bool)))
               ((op-p expr)
                  (let* ((op1 (op-op1 expr)) (op2 (op-op2 expr)) (op (op-op expr))
                         (typ-oper (type-operands op forced-types)) (typ-op (type-op op forced-types)))
                     (when (no-types-p typ-op)
                        (error 'type-invalid-error :text "no types error for result or operands"))
                     (let ((t1 (get-type op1 typ-oper)) (t2 (get-type op2 typ-oper)))
                        (when (< (length t1) (length t2))
                           (setf t2 (get-type op2 t1)))
                        (when (< (length t2) (length t1))
                           (setf t1 (get-type op1 t2)))
                        (when (and (= (length t1) 2) (one-elem-p forced-types) (eq (first forced-types) :type-bool))
                           (setf t1 (get-type op1 (select-simpler-types t1)))
                           (setf t2 (get-type op2 (select-simpler-types t2))))
                        (type-oper-op op t1))))
               (t (error 'type-invalid-error :text (tostring "Unknown expression ~a to typecheck" expr))))))
      (let ((types (do-get-type expr forced-types)))
         (when (no-types-p types)
            (error 'type-invalid-error :text (tostring "Type error in expression ~a: wanted types ~a" expr forced-types)))
         (set-type expr types)
         types)))
      
(defun do-type-check-subgoal (name args options &key (body-p nil) (axiom-p nil))
   (let* ((def (lookup-definition name))
          (definition (definition-types def)))
      (unless def
         (error 'type-invalid-error :text (concatenate 'string "Definition " name " not found")))
      (when (not (= (length definition) (length args)))
         (error 'type-invalid-error :text (tostring "Invalid number of arguments in subgoal ~a~a" name args)))
      (cond
         ((is-linear-p def) ;; linear fact
            (dolist (opt options)
               (case opt
                  (:reuse
                     (unless body-p
                        (error 'type-invalid-error :text (tostring "Linear reuse of facts must be used in the body, not the head: ~a" name))))
                  (:persistent
                     (error 'type-invalid-error :text (tostring "Only persistent facts may use !: ~a" name)))
                  (otherwise
                     (error 'type-invalid-error :text (tostring "Unrecognized option ~a for subgoal ~a" opt name))))))
         (t ;; persistent fact
            (let ((has-persistent-p nil))
               (dolist (opt options)
                  (case opt
                     (:reuse
                        (error 'type-invalid-error :text (tostring "Reuse option $ may only be used with linear facts: ~a" name)))
                     (:persistent
                        (setf has-persistent-p t))
                     (otherwise
                        (error 'type-invalid-error :text (tostring "Unrecognized option ~a for subgoal ~a" opt name)))))
               (unless has-persistent-p
                  (warn (tostring "Subgoal ~a needs to have a !" name))))))
      (dolist2 (arg args) (forced-type (definition-arg-types definition))
         (when (and body-p (not (var-p arg)))
            (error 'type-invalid-error :text (tostring "only variables at body: ~a" arg)))
         (unless (one-elem-p (get-type arg `(,forced-type)))
            (error 'type-invalid-error :text "type error"))
         (when (var-p arg)
            (if (and (not body-p) (not (variable-defined-p arg)))
               (error 'type-invalid-error :text (tostring "undefined variable: ~a" arg)))
            (if body-p
               (variable-is-defined arg))))))

(defun do-type-check-constraints (expr)
   (unless (has-variables-defined expr)
      (error 'type-invalid-error :text "all variables must be defined"))
   (let ((typs (get-type expr '(:type-bool))))
      (unless (and (one-elem-p typs) (type-bool-p (first typs)))
         (error 'type-invalid-error :text "constraint must be of type bool"))))

(defun update-assignment (assignments assign)
   (let* ((var (assignment-var assign)) (var-name (var-name var)))
      (multiple-value-bind (forced-types ok) (gethash var-name *constraints*)
         (let ((ty (get-type (assignment-expr assign) (if ok forced-types *all-types*))))
            (variable-is-defined var)
            (force-constraint var-name ty)
            (set-type var ty)
            (dolist (used-var (all-variables (assignment-expr assign)))
               (when-let ((other (find-if #'(lambda (a)
                                             (and (var-eq-p used-var (assignment-var a))
                                                   (not (one-elem-p (expr-type (assignment-var a))))))
                                    assignments)))
                  (update-assignment assignments other)))))))
                  
(defun assert-assignment-undefined (assignments)
   (unless (every #'(lambda (a) (not (variable-defined-p a))) (get-assignment-vars assignments))
      (error 'type-invalid-error :text "some variables are already defined")))

(defun do-type-check-assignments (body test)
   (let ((assignments (get-assignments body)))
      (loop until (every #'(lambda (a) (and (funcall test (assignment-var a)))) assignments)
            for assign = (find-if #'(lambda (a)
                                       (and (not (funcall test (assignment-var a)))
                                          (has-variables-defined (assignment-expr a))))
                                 assignments)
            do (unless assign
                  (error 'type-invalid-error :text "undefined variables"))
               (when (< 1 (count-if #L(var-eq-p (assignment-var assign) !1) (get-assignment-vars assignments)))
                  (error 'type-invalid-error :text "cannot set multiple variables"))
               (update-assignment assignments assign))))

(defun create-assignments (body)
   "Turn undefined equal constraints to assignments"
   (let (vars)
      (do-constraints body (:expr expr :constraint orig)
         (let ((op1 (op-op1 expr)) (op2 (op-op2 expr)))
            (when (and (op-p expr) (equal-p expr) (var-p op1)
                        (not (variable-defined-p op1))
                        (not (has-elem-p vars (var-name op1))))
         ;; changes constraints to assignments
         (setf (first orig) :assign)
         (setf (second orig) op1)
         (setf (cddr orig) (list op2))
         (push (var-name op1) vars))))))
         
(defun unfold-cons (mangled-var cons clause)
   (let ((tail-var (generate-random-var))
         (tail (cons-tail cons)))
      (push (make-constraint (make-not (make-test-nil mangled-var)) 100) (clause-body clause))
      (push (make-constraint (make-equal (cons-head cons) '= (make-head mangled-var))) (clause-body clause))
      (cond
         ((cons-p tail)
            (push (make-constraint (make-equal tail-var '= (make-tail mangled-var))) (clause-body clause))
            (unfold-cons tail-var tail clause))
         (t
            (push (make-constraint (make-equal tail '= (make-tail mangled-var))) (clause-body clause))))))

(defun transform-constant-to-constraint (clause arg &optional only-addr-p)
   (cond ((const-p arg)
            (letret (new-var (generate-random-var))
               (if (and (not only-addr-p) (cons-p arg))
                  (unfold-cons new-var arg clause)
                  (push (make-constraint (make-equal new-var '= arg)) (clause-body clause)))))
          (t arg)))
(defun transform-constants-to-constraints (clause args &optional only-addr-p)
   (mapcar #L(transform-constant-to-constraint clause !1 only-addr-p) args))

(defun add-variable-head-clause (clause)
   (do-subgoals (clause-head clause) (:args args :subgoal sub)
      (setf (first (subgoal-args sub))
               (transform-constant-to-constraint clause
                     (first args)))))
                     
(defun add-variable-head ()
   (do-rules (:clause clause)
      (add-variable-head-clause clause))
   (do-axioms (:clause clause)
      (add-variable-head-clause clause)))
      
(defun do-type-check-comprehension (comp)
   (let ((old-defined *defined*)
         (target-variables (mapcar #'var-name (comprehension-variables comp))))
      (extend-typecheck-context
         (with-comprehension comp (:left left :right right)
            (do-subgoals left (:name name :args args :options options)
               (do-type-check-subgoal name args options :body-p t))
            (do-subgoals right (:name name :args args :options options)
               (do-type-check-subgoal name args options)))
         ;; check if the set of new defined variables is identical to target-variables
         (let ((new-ones *defined-in-context*))
            (unless (subsetp new-ones target-variables)
               (error 'type-invalid-error :text (tostring "Comprehension ~a is using more variables than it specifies" comp)))
            (unless (subsetp target-variables new-ones)
               (error 'type-invalid-error :text (tostring "Comprehension ~a is not using enough variables" comp)))))))
      
(defun type-check-clause (head body clause axiom-p)
   (with-typecheck-context
      (when axiom-p
         (variable-is-defined (first-host-node head)))
      (do-subgoals body (:name name :args args :options options)
         (do-type-check-subgoal name args options :body-p t))
      (create-assignments body)
      (assert-assignment-undefined (get-assignments body))
      (do-type-check-assignments body #'typed-var-p)
      ;(unless (every #'variable-defined-p (all-variables (append head body)))
      ;   (error 'type-invalid-error :text (tostring "undefined variables in ~a" (append head body))))
      (do-subgoals head (:name name :args args :options options)
         (do-type-check-subgoal name args options :axiom-p axiom-p))
      (do-comprehensions head (:comp comp)
         (do-type-check-comprehension comp))
      (do-constraints body (:expr expr)
         (do-type-check-constraints expr))
      (do-type-check-assignments
         (setf (clause-body clause) (remove-unneeded-assignments body head))
         #'single-typed-var-p)))

(defun transform-clause-constants (clause)
   (do-subgoals (clause-body clause) (:args args :subgoal sub)
      (setf (subgoal-args sub) (transform-constants-to-constraints clause args))))
            
(defun type-check ()
   (do-definitions (:name name :types typs)
      (check-home-argument name typs)
      (check-aggregates name typs))
   (add-variable-head)
   (do-rules (:clause clause)
      (transform-clause-constants clause))
   (do-axioms (:clause clause)
      (transform-clause-constants clause))
   (do-all-rules (:head head :body body :clause clause)
      (type-check-clause head body clause nil))
   (do-all-axioms (:head head :body body :clause clause)
      (type-check-clause head body clause t)))
