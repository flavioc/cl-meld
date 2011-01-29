
(defun definitions (code) (second code))
(defun clauses (code) (fourth code))
(defun clause-head (clause) (third clause))
(defun clause-body (clause) (first clause))
(defun constraint-p (ls) (tagged-p ls :constraint))
(defun constraint-expr (ls) (second ls))
(defun subgoal-p (ls) (and (listp ls) (not (constraint-p ls))))
(defun subgoal-name (subgoal) (first subgoal))
(defun subgoal-args (subgoal) (second subgoal))

(defun lookup-definition (defs pred)
   (rest (assoc-if #'(lambda (sub) (string-equal pred sub))
            defs)))
            
(defun type-int-p (type) (eq :type-int type))
(defun type-node-p (type) (eq :type-node type))
(defun type-bool-p (type) (eq :type-bool type))
(defun type-float-p (type) (eq :type-float type))
(defun type-any-p (type) (eq :type-any type))

(defun has-constraints (subgoals) (some #'constraint-p subgoals))

(defmacro define-ops (&rest symbs)
   `(progn
      ,@(mapcar #'(lambda (sy)
            `(defun ,(intern (concatenate 'string (symbol-name sy) "-P")) (val)
                  (tagged-p val ,sy)))
         symbs)))
         
(define-ops :int :var :plus :minus :mul :div :mod
            :lesser :lesser-equal :greater :greater-equal)

(defun op-op (val) (tagged-tag val))
(defun op-op1 (val) (second val))
(defun op-op2 (val) (third val))

(defun op-p (val)
   (any (plus-p minus-p mul-p div-p mod-p lesser-p lesser-equal-p greater-p greater-equal-p) val))
   
(defun op-to-string (op)
   (case op
      (:plus "+")
      (:minus "-")
      (:mul "*")
      (:div "/")
      (:mod "%")
      (:lesser "<")
      (:lesser-equal "<=")
      (:greater ">")
      (:greater-equal ">=")))
      
(defun type-operands (op &optional forced-type)
   (case op
      ((:plus :minus :mul :div :mod)
         (case forced-type
            (:type-int '(:type-int))
            (:type-float '(:type-float))
            (otherwise '(:type-int :type-float))))
      ((:lesser :lesser-equal :greater :greater-equal)
         (case forced-type
            ((:type-node :type-int :type-float) nil)
            (otherwise '(:type-int :type-float))))))
      
(defun type-op (op &optional forced-type)
   (case op
      ((:plus :minus :mul :div :mod)
         (case forced-type
            (:type-int '(:type-int))
            (:type-float '(:type-float))
            (otherwise '(:type-int :type-float))))
      ((:lesser :lesser-equal :greater :greater-equal)
         (case forced-type
            ((:type-node :type-float :type-int) nil)
            (otherwise '(:type-bool))))))
            
(defun var-name (val) (second val))
(defun int-val (val) (second val))