
(defun definitions (code) (second code))
(defun clauses (code) (fourth code))
(defun clause-head (clause) (third clause))
(defun clause-body (clause) (first clause))
(defun constraint-p (ls) (tagged-p ls :constraint))
(defun constraint-expr (ls) (second ls))

(defun assignment-p (ls) (tagged-p ls :assign))
(defun assignment-var (ls) (second ls))
(defun assignment-expr (ls) (third ls))

(defun subgoal-p (ls) (tagged-p ls :subgoal))
(defun subgoal-name (subgoal) (second subgoal))
(defun subgoal-args (subgoal) (third subgoal))

(defun get-assignments (body) (remove-if-not #'assignment-p body))
(defun get-assignment-vars (assignments) (mapcar #'assignment-var assignments))

(defun typed-var-p (var) (= (length var) 3))

(defun var-eq-p (v1 v2) (equal (var-name v1) (var-name v2)))

(defun expr-type (expr)
   (cond
      ((or (var-p expr) (int-p expr)) (third expr))
      ((op-op expr) (fourth expr))))

(defun lookup-definition (defs pred)
   (rest (assoc-if #'(lambda (sub) (string-equal pred sub))
            defs)))
            
(defparameter *all-types* '(:type-int :type-float :type-bool :type-node))

(defun type-int-p (type) (eq :type-int type))
(defun type-node-p (type) (eq :type-node type))
(defun type-bool-p (type) (eq :type-bool type))
(defun type-float-p (type) (eq :type-float type))

(defun has-constraints (subgoals) (some #'constraint-p subgoals))
(defun has-assignments (subgoals) (some #'assignment-p subgoals))

(defmacro define-ops (&rest symbs)
   `(progn
      ,@(mapcar #'(lambda (sy)
            `(defun ,(intern (concatenate 'string (symbol-name sy) "-P")) (val)
                  (tagged-p val ,sy)))
         symbs)))
         
(define-ops :int :var :plus :minus :mul :div :mod
            :equal :lesser :lesser-equal :greater :greater-equal)

(defun op-op (val) (tagged-tag val))
(defun op-op1 (val) (second val))
(defun op-op2 (val) (third val))

(defun op-p (val)
   (any (plus-p minus-p mul-p div-p mod-p equal-p lesser-p lesser-equal-p greater-p greater-equal-p) val))
   
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
      
(defun type-operands (op &optional forced-types)
   (case op
      ((:plus :minus :mul :div :mod)
         (if forced-types
            (intersection forced-types '(:type-int :type-float))
            '(:type-int :type-float)))
      ((:equal :lesser :lesser-equal :greater :greater-equal)
         (if (or forced-types
                 (not (has-elem-p forced-types :type-bool)))
            '(:type-int :type-float)))))
            
(defun type-op (op &optional forced-types)
   (case op
      ((:plus :minus :mul :div :mod)
         (if forced-types
            (intersection '(:type-int :type-float) forced-types)
            '(:type-int :type-float)))
      ((:equal :lesser :lesser-equal :greater :greater-equal)
         (if forced-types
            (intersection '(:type-bool) forced-types)
            '(:type-bool)))))
            
(defun type-oper-op (op forced-types)
   (case op
      ((:plus :minus :mul :div :mod)
         (intersection '(:type-int :type-float) forced-types))
      ((:equal :lesser :lesser-equal :greater :greater-equal) '(:type-bool))))
            
(defun var-name (val) (second val))
(defun int-val (val) (second val))