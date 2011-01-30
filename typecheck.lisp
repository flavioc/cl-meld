
(define-condition tuple-no-arguments-error (error)
  ((text :initarg :text :reader text)))
(define-condition tuple-no-home-argument-error (error)
  ((text :initarg :text :reader text)))
(define-condition type-invalid-error (error)
  ((text :initarg :text :reader text)))
(define-condition type-definition-not-found-error (error)
   ((text :initarg :text :reader text)))
(define-condition type-number-of-args-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun check-home-argument (name typs)
   (when (null typs)
      (error 'tuple-no-arguments-error :text (concatenate 'string name " has no arguments")))
   (unless (type-node-p (first typs))
      (error 'tuple-no-home-argument-error
         :text (concatenate 'string "first argument of tuple "
                  name " must be of type 'node'"))))

(defun set-type (expr typs)
   (when (one-elem-p typs)
      (setf typs (first typs))
      (cond
         ((or (var-p expr) (int-p expr)) (setf (cddr expr) (list typs)))
         ((op-p expr) (setf (cdddr expr) (list typs))))))
         
(defun make-constraints () (make-hash-table))
(defun get-type (constraints expr &optional forced-types)
   (let (typ-res)
      (cond
         ((var-p expr)
            (setf typ-res (if forced-types forced-types *all-types*))
            (setf typ-res (force-constraint constraints (var-name expr) typ-res)))
         ((int-p expr)
            (format t "int forced type ~A~%" forced-types)
            (setf typ-res
               (intersection forced-types '(:type-int :type-float))))
         ((op-p expr)
            (let* ((op1 (op-op1 expr)) (op2 (op-op2 expr)) (op (op-op expr))
                  (typ-op (type-operands op forced-types)))
               (setf typ-res (type-op op forced-types))
               (when (or (no-types-p typ-op) (no-types-p typ-res))
                  (error 'type-invalid-error :text "no types error for result or operands"))
               (get-type constraints op1 typ-op)
               (get-type constraints op2 typ-op))))
         (set-type expr typ-res)
         typ-res))

(defun no-types-p (ls) (null ls))
(defun invalid-type-combination-p (types)
   (let ((num (or (has-elem-p types :type-int) (has-elem-p types :type-float)))
         (bool (has-elem-p types :type-bool))
         (node (has-elem-p types :type-node)))
      (> (count t (list num bool node)) 1)))
            
(defun merge-types (ls types)
   (let ((ret (intersection ls types)))
      (if (invalid-type-combination-p ret)
         nil ret)))
(defun merge-type (ls type) (merge-types ls `(,type)))

               
(defun force-constraint (constraints var new-types)
   (multiple-value-bind (types ok) (gethash var constraints)
      (when ok
         (setf new-types (merge-types types new-types))
         (when (no-types-p new-types)
            (error 'type-invalid-error :text "type error")))
      (format t "new types for ~A are ~A~%" var new-types)
      (setf (gethash var constraints) new-types)
      new-types))
      
(defun do-type-check-subgoals (defs subgoals constraints)
   (do-subgoals subgoals (name args)
      (let ((definition (lookup-definition defs name)))
         (unless definition
            (error 'type-definition-not-found-error :text "definition not found"))
         (when (not (= (length definition) (length args)))
            (error 'type-number-of-args-invalid-error :text "invalid number of arguments"))
         (dolist2 (arg args) (forced-type definition)
            (let ((res-type (get-type constraints arg `(,forced-type))))
               (when (no-types-p res-type)
                  (error 'type-invalid-error :text "type error")))))))
                  
(defun do-type-check-constraints (constraints expr)
   (let ((typs (get-type constraints expr '(:type-bool))))
      (unless (and (one-elem-p typs) (type-bool-p (first typs)))
         (error 'type-invalid-error :text "constraint must be of type bool"))))
   
(defun type-check (code)
   (do-definitions code (name typs)
      (check-home-argument name typs))
   (do-clauses code (head body)
      (let ((constraints (make-constraints))
            (subgoals (append head body)))
         (do-type-check-subgoals (definitions code)
                                 subgoals
                                 constraints)
         (do-constraints body (expr)
            (do-type-check-constraints constraints expr)))))
         