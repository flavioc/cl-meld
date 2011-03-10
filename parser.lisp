
(in-package :cl-meld)

(define-string-lexer meld-lexer
 	("type"			(return (values :type $@)))
 	("extern"      (return (values :extern $@)))
 	("const"       (return (values :const-decl $@)))
	("int"			(return (values :type-int $@)))
	("float"       (return (values :type-float $@)))
	("catom"		   (return (values :type-catom $@)))
	("node"        (return (values :type-node $@)))
	(":-"          (return (values :arrow  $@)))
	("\\("			(return (values :lparen $@)))
	("\\)"			(return (values :rparen $@)))
	("\\."         (return (values :dot $@)))
	("\\,"         (return (values :comma $@)))
	("\\+"         (return (values :plus $@)))
	("\\-"         (return (values :minus $@)))
	("\\*"         (return (values :mul $@)))
	("\\%"         (return (values :mod $@)))
	("\\/"         (return (values :div $@)))
	("\\<="        (return (values :lesser-equal $@)))
	("\\<"         (return (values :lesser $@)))
	("\\>"         (return (values :greater $@)))
	("\\>="        (return (values :greater-equal $@)))
	("\\="         (return (values :equal $@)))
	("min"         (return (values :min $@)))
	("max"         (return (values :max $@)))
	("sum"         (return (values :sum $@)))
	("first"       (return (values :first $@)))
	("[-+]?[0-9]+(\.[0-9]+|[0-9]+)?" (return (values :number $@)))
	("_"				(return (values :variable $@)))
 	("[a-z]([a-z]|\_)*"		(return (values :const $@)))
	("'\\w+"		(return (values :const $@)))
	("[A-Z]([A-Z]|[a-z]|[0-9])*"	(return (values :variable $@))))

(defun make-var-parser (var)
 (if (eq var '_)
	(generate-random-var)
	(make-var var)))
	
(defun parse-number (str)
   (if (find #\. str)
      (make-float (read-from-string str))
      (make-int (parse-integer str))))
	
(defun make-const-definition (name expr) `(:const ,name ,expr))
(defun const-definition-p (const) (tagged-p const :const))
(defun const-definition-name (const) (second const))
(defun const-definition-expr (const) (third const))

(defparameter *parsed-consts* nil)
(defun lookup-const-def (name)
   (const-definition-expr (find-if #L(equal (const-definition-name !1) name) *parsed-consts*)))
   
(defmacro return-const (const)
   `#'(lambda (x) (declare (ignore x)) ,const))

(define-parser meld-parser
   (:muffle-conflicts t)
 	(:start-symbol program)
	(:terminals (:const :type :variable :number :lparen :rparen
								:bar :arrow :dot :comma :type-int :type-node
								:type-catom :type-float :plus :minus :mul :mod :div
								:lesser :lesser-equal :greater :greater-equal :equal
								:extern :const-decl :min :max :first :sum))
	(program
	  (definitions statements #L(make-ast !1 !2)))

	(definitions
	 (definition #'list)
	 (definition definitions #L(if !1 (cons !1 !2) !2)))

	(definition
	 (:extern atype const :lparen type-args :rparen :dot #'(lambda (e ret-type name l args r d)
	                                                         (declare (ignore e l r d))
	                                                         (make-extern name ret-type args)))
	 (:const-decl const :equal expr :dot #'(lambda (a name e expr dot)
	                                             (declare (ignore a e dot))
	                                             (push (make-const-definition name expr) *parsed-consts*)
	                                             nil))
	 (:type const :lparen type-args :rparen :dot #'(lambda (ty const l typs r d)
																		(declare (ignore ty l r d))
																		(make-definition const typs))))

	(type-args
	 (type-decl #'list)
	 (type-decl :comma type-args #'(lambda (ty comma ls) (declare (ignore comma)) (cons ty ls))))

   (type-decl
    (atype #'identity)
    (aggregate-decl atype #'make-aggregate))
    
   (aggregate-decl
    (:min (return-const :min))
    (:max (return-const :max))
    (:first (return-const :first))
    (:sum (return-const :sum)))
    
	(atype
	 (:type-int #'(lambda (x) (declare (ignore x)) :type-int))
	 (:type-float #'(lambda (x) (declare (ignore x)) :type-float))
	 (:type-catom #'(lambda (x) (declare (ignore x)) :type-node))
	 (:type-node #'(lambda (x) (declare (ignore x)) :type-node)))

	(statements
	 (statement #'list)
	 (statement statements #'cons))

	(statement
	   (head-terms :dot #'(lambda (head d) (declare (ignore d)) (make-clause nil head)))
		(head-terms :arrow body-terms :dot #'(lambda (conc y perm w) (declare (ignore y w)) (make-clause perm conc))))

   (head-terms
      (term #'list)
      (term :comma head-terms #'(lambda (x y z) (declare (ignore y)) (cons x z))))

   (body-terms
      (body-term #'list)
      (body-term :comma body-terms #'(lambda (x y z) (declare (ignore y)) (cons x z))))

   (body-term
      (term #'identity)
      (constraint #'identity))

	(term
	 	(const :lparen args :rparen #'(lambda (name y args w) (declare (ignore y w)) (make-subgoal name args))))

   (constraint
      (cmp #'(lambda (c) (make-constraint c))))

	(args
	 	(expr #'list)
		(expr :comma args #'(lambda (x y z) (declare (ignore y)) (cons x z))))

	(expr
	   variable
	   (const :lparen args :rparen #'(lambda (name l args r) (declare (ignore l r)) (make-call name args)))
	   (const #L(lookup-const-def !1))
		(:number #L(parse-number !1))
	   (:lparen expr :rparen #'(lambda (l expr r) (declare (ignore l r)) expr))
	   (expr :minus expr #'make-minus)
	   (expr :mul expr #'make-mul)
	   (expr :mod expr #'make-mod)
	   (expr :div expr #'make-div)
	   (expr :plus expr #'make-plus))

   (cmp
      (expr :equal expr #'make-equal)
      (expr :lesser expr #'make-lesser)
      (expr :lesser-equal expr #'make-lesser-equal)
      (expr :greater expr #'make-greater)
      (expr :greater-equal expr #'make-greater-equal))
      
	(variable
	 (:variable (lambda (x) (make-var-parser x))))

	(const
	 (:const #'identity)))
      
(defun parse-meld (str)
 (let* ((lexer (meld-lexer str))
        (*parsed-consts* nil))
   (parse-with-lexer lexer meld-parser)))