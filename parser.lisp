
(in-package :cl-meld)

(define-string-lexer meld-lexer
 	("type"			(return (values :type $@)))
 	("extern"      (return (values :extern $@)))
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
	("\\d+"		   (return (values :number $@)))
	("_"				(return (values :variable $@)))
 	("[a-z]+"		(return (values :const $@)))
	("'\\w+'"		(return (values :const $@)))
	("[A-Z]([A-Z]|[a-z]|[0-9])*"	(return (values :variable $@))))

(defun make-var-parser (var)
 (if (eq var '_)
	(generate-random-var)
	(make-var var)))

(define-parser meld-parser
   (:muffle-conflicts t)
 	(:start-symbol program)
	(:terminals (:const :type :variable :number :lparen :rparen
								:bar :arrow :dot :comma :type-int :type-node
								:type-catom :type-float :plus :minus :mul :mod :div
								:lesser :lesser-equal :greater :greater-equal :equal
								:extern))
	(program
	  (definitions statements (lambda (x y) (list :definitions x :clauses y))))

	(definitions
	 (definition #'list)
	 (definition definitions #'cons))

	(definition
	 (:type const :lparen type-args :rparen :dot #'(lambda (ty const l typs r d)
																		(declare (ignore ty l r d))
																		(make-definition const typs))))

	(type-args
	 (atype #'list)
	 (atype :comma type-args #'(lambda (ty comma ls) (declare (ignore comma)) (cons ty ls))))

	(atype
	 (:type-int #'(lambda (x) (declare (ignore x)) :type-int))
	 (:type-float #'(lambda (x) (declare (ignore x)) :type-float))
	 (:type-catom #'(lambda (x) (declare (ignore x)) :type-node))
	 (:type-node #'(lambda (x) (declare (ignore x)) :type-node)))

	(statements
	 (statement #'list)
	 (statement statements #'cons))

	(statement
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
		(expr :comma args (lambda (x y z) (declare (ignore y)) (cons x z))))

	(expr
	   variable
		(:number #L(make-int (parse-integer !1)))
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
 (let ((lexer (meld-lexer str)))
	(parse-with-lexer lexer meld-parser)))
