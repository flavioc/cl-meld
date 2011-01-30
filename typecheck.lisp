(define-condition type-invalid-error (error)
   ((text :initarg :text :reader text)))
   
(defparameter *constraints* nil)

(defun check-home-argument (name typs)
   (when (null typs)
      (error 'type-invalid-error :text (concatenate 'string name " has no arguments")))
   (unless (type-node-p (first typs))
      (error 'type-invalid-error
         :text (concatenate 'string "first argument of tuple "
                  name " must be of type 'node'"))))

(defun set-type (expr typ)
   (cond
      ((or (var-p expr) (int-p expr)) (setf (cddr expr) (list typ)))
      ((op-p expr) (setf (cdddr expr) (list typ)))))
         
(defun get-type (expr &optional forced-types)
   (let (typ-res)
      (cond
         ((var-p expr)
            (setf typ-res (if forced-types forced-types *all-types*))
            (setf typ-res (force-constraint (var-name expr) typ-res)))
         ((int-p expr)
            (setf typ-res
               (intersection forced-types '(:type-int :type-float))))
         ((op-p expr)
            (let* ((op1 (op-op1 expr)) (op2 (op-op2 expr)) (op (op-op expr))
                  (typ-op (type-operands op forced-types)))
               (setf typ-res (type-op op forced-types))
               (when (or (no-types-p typ-op) (no-types-p typ-res))
                  (error 'type-invalid-error :text "no types error for result or operands"))
               (get-type op1 typ-op)
               (get-type op2 typ-op))))
         (set-type expr (first typ-res))
         typ-res))

(defun no-types-p (ls) (null ls))
(defun valid-type-combination-p (types)
   (let ((num (or (has-elem-p types :type-int) (has-elem-p types :type-float)))
         (bool (has-elem-p types :type-bool))
         (node (has-elem-p types :type-node)))
      (= (count t (list num bool node)) 1)))
            
(defun merge-types (ls types)
   (iff valid-type-combination-p (intersection ls types)))

(defun merge-type (ls type) (merge-types ls `(,type)))
               
(defun force-constraint (var new-types)
   (multiple-value-bind (types ok) (gethash var *constraints*)
      (when ok
         (setf new-types (merge-types types new-types))
         (when (no-types-p new-types)
            (error 'type-invalid-error :text "type error")))
      (setf (gethash var *constraints*) new-types)
      new-types))
      
(defun do-type-check-subgoal (defs name args)
   (let ((definition (lookup-definition defs name)))
      (unless definition
         (error 'type-invalid-error :text "definition not found"))
      (when (not (= (length definition) (length args)))
         (error 'type-invalid-error :text "invalid number of arguments"))
      (dolist2 (arg args) (forced-type definition)
         (let ((res-type (get-type arg `(,forced-type))))
            (when (no-types-p res-type)
               (error 'type-invalid-error :text "type error"))))))
                  
(defun do-type-check-constraints (expr)
   (let ((typs (get-type expr '(:type-bool))))
      (unless (and (one-elem-p typs) (type-bool-p (first typs)))
         (error 'type-invalid-error :text "constraint must be of type bool"))))
   
(defun type-check (code)
   (do-definitions code (name typs)
      (check-home-argument name typs))
   (do-clauses code (head body)
      (let ((*constraints* (make-hash-table))
            (definitions (definitions code)))
         (do-subgoals (append head body) (name args)
            (do-type-check-subgoal definitions name args))
         (do-constraints body (expr)
            (do-type-check-constraints expr)))))
         