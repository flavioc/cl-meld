(in-package :cl-meld)

(defmacro define-makes (&rest symbs)
   `(on-top-level
      ,@(mapcar #'(lambda (sym)
         `(defun ,(intern (concatenate 'string "MAKE-" (symbol-name sym))) (a b c)
               (declare (ignore b))
               (list ,sym a c)))
            symbs)))

(define-makes :plus :minus :mul :mod :div
      :lesser :lesser-equal :greater :greater-equal :equal :assign)
   
(defun make-clause (perm conc &rest options) `(:clause ,perm ,conc ,options))
(defun clause-head (clause) (third clause))
(defun clause-body (clause) (second clause))
(defun set-clause-body (clause new-body)
   (setf (second clause) new-body))
(defsetf clause-body set-clause-body)

(defun clause-options (clause) (fourth clause))
(defun clause-add-option (clause opt) (push opt (fourth clause))) 

(defun make-subgoal (name args) (list :subgoal name args))
(defun make-var (var &optional typ) `(:var ,(if (stringp var) (str->sym var) var) ,@(if typ `(,typ) nil)))
(defun make-definition (name typs &rest options) `(:definition ,name ,typs ,options))
(defun make-constraint (expr) (list :constraint expr))
(defun definition-name (def) (second def))
(defun definition-types (def) (third def))
(defun definition-options (def) (fourth def))
(defun definition-add-option (def opt) (push opt (fourth def)))

(defmacro define-ops (&rest symbs)
   `(on-top-level
      ,@(mapcar #'(lambda (sy)
            `(defun ,(intern (concatenate 'string (symbol-name sy) "-P")) (val)
                  (tagged-p val ,sy)))
         symbs)))
         
(define-ops :int :var :plus :minus :mul :div :mod
            :equal :lesser :lesser-equal :greater :greater-equal)
            
            
(defun const-p (s) (or (int-p s)))
            
(defun op-op (val) (tagged-tag val))
(defun op-op1 (val) (second val))
(defun op-op2 (val) (third val))

(defun op-p (val)
   (any (plus-p minus-p mul-p div-p mod-p equal-p lesser-p lesser-equal-p greater-p greater-equal-p) val))

(defun int-val (val) (second val))
(defun make-int (int) `(:int ,int))

(defun var-name (val) (second val))
(defun var-eq-p (v1 v2) (equal (var-name v1) (var-name v2)))

(defun typed-var-p (var) (and (= (length var) 3)))
(defun single-typed-var-p (var) (and (typed-var-p var) (one-elem-p (third var))))
(defun typed-op-p (op) (= (length op) 4))
(defun typed-int-p (i) (= (length i) 3))
            
(defun definitions (code) (second code))
(defun set-definitions (code new-defs)
   (setf (second code) new-defs))
(defsetf definitions set-definitions)
  
(defun clauses (code) (fourth code))
(defun set-clauses (code new-clauses)
   (setf (fourth code) new-clauses))
(defsetf clauses set-clauses)

(defun constraint-p (ls) (tagged-p ls :constraint))
(defun constraint-expr (ls) (second ls))

(defun assignment-p (ls) (tagged-p ls :assign))
(defun assignment-var (ls) (second ls))
(defun assignment-expr (ls) (third ls))

(defun subgoal-p (ls) (tagged-p ls :subgoal))
(defun subgoal-name (subgoal) (second subgoal))
(defun subgoal-args (subgoal) (third subgoal))
(defun set-subgoal-args (subgoal new-args)
   (setf (third subgoal) new-args))
(defsetf subgoal-args set-subgoal-args)

(defun get-assignments (body) (filter #'assignment-p body))
(defun get-assignment-vars (assignments) (mapcar #'assignment-var assignments))
(defun get-subgoals (code) (filter #'subgoal-p code))
(defun get-constraints (code) (remove-if-not #'constraint-p code))

(defun expr-type (expr)
   (cond
      ((or (var-p expr) (int-p expr)) (third expr))
      ((op-op expr) (fourth expr))))

(defun lookup-definition-types (defs pred)
   (when-let ((def (lookup-definition defs pred)))
      (definition-types def)))
      
(defun lookup-definition (defs pred) (find-if #'(lambda (d) (string-equal pred (definition-name d))) defs))
         
(defparameter *all-types* '(:type-int :type-float :type-bool :type-node))

(defmacro deftype-p (&rest types)
   `(on-top-level
         ,@(mapcar #'(lambda (x) `(defun ,(format-symbol t "TYPE-~A-P" (symbol-name x)) (ty)
                                       (eq ,(format-symbol "KEYWORD" "TYPE-~A" (symbol-name x)) ty)))
                  types)))

(deftype-p int node bool float)

(defun has-constraints (subgoals) (some #'constraint-p subgoals))
(defun has-assignments (subgoals) (some #'assignment-p subgoals))
   
(defun op-to-string (op)
   (case op
      (:plus "+")
      (:minus "-")
      (:mul "*")
      (:div "/")
      (:mod "%")
      (:equal "==")
      (:lesser "<")
      (:lesser-equal "<=")
      (:greater ">")
      (:greater-equal ">=")))
      
(defmacro eq-or (sym &rest symbols)
   `(or ,@(mapcar #'(lambda (s) `(eq ,sym ,s)) symbols)))
   
(defun eq-arith-p (sym) (eq-or sym :plus :minus :mul :div :mod))
(defun eq-cmp-p (sym) (eq-or sym :equal :lesser :lesser-equal :greater :greater-equal))
      
(defun type-operands (op &optional forced-types)
   (cond
      ((eq-arith-p op)
         (if forced-types
            (intersection forced-types '(:type-int :type-float))
            '(:type-int :type-float)))
      ((eq-cmp-p op)
         (if (or forced-types
                 (not (has-elem-p forced-types :type-bool)))
            '(:type-int :type-float)))))

(defun type-op (op &optional forced-types)
   (cond
      ((eq-arith-p op)
         (if forced-types
            (intersection '(:type-int :type-float) forced-types)
            '(:type-int :type-float)))
      ((eq-cmp-p op)
         (if forced-types
            (intersection '(:type-bool) forced-types)
            '(:type-bool)))))
            
(defun type-oper-op (op forced-types)
   (cond
      ((eq-arith-p op)
         (intersection '(:type-int :type-float) forced-types))
      ((eq-cmp-p op) '(:type-bool))))
      

(defun all-variables (expr)
 (cond
   ((subgoal-p expr) (reduce #'(lambda (old arg) (dunion old (all-variables arg))) (subgoal-args expr) :initial-value nil))
   ((constraint-p expr) (all-variables (constraint-expr expr)))
   ((assignment-p expr) (dunion (list (assignment-var expr)) (all-variables (assignment-expr expr))))
   ((var-p expr) (list expr))
   ((int-p expr) nil)
   ((op-p expr) (dunion (all-variables (op-op1 expr)) (all-variables (op-op2 expr))))
   ((listp expr) (reduce #'(lambda (old arg) (dunion old (all-variables arg))) expr :initial-value nil))))
   
(defun all-variable-names (expr) (mapcar #'var-name (all-variables expr)))

(defparameter *var-counter* 0)
(defun generate-random-var () (make-var (with-output-to-string (a) (format a "Mangledvar~a" (incf *var-counter*)))))

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