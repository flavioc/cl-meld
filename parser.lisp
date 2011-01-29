
(in-package :cl-meld)

(define-string-lexer meld-lexer
 	("type"			(return (values :type $@)))
	("int"			(return (values :type-int $@)))
	("catom"		(return (values :type-catom $@)))
	(":-"           (return (values :arrow  $@)))
	("\\("			(return (values :lparen $@)))
	("\\)"			(return (values :rparen $@)))
	("\\."          (return (values :dot $@)))
	("\\,"          (return (values :comma $@)))
	("\\d+"		(return (values :number $@)))
	("_"				(return (values :variable $@)))
 	("[a-z]+"		(return (values :const $@)))
	("'\\w+'"		(return (values :const $@)))
	("[A-Z]([A-Z]|[a-z])*"	(return (values :variable $@))))

(defun str->sym (str)
 (values (intern str)))

(defun make-var (var)
 (if (eq var '_)
	:placeholder
	`(:var ,var)))
	
(defun make-int (int)
   (list :int (parse-integer int)))
			
(define-parser meld-parser
 	(:start-symbol program)
	(:terminals (:const :type :variable :number :lparen :rparen
								:bar :arrow :dot :comma :type-int :type-catom))
	(program
	  (definitions statements (lambda (x y) (list :definitions x :clauses y))))

	(definitions
	 (definition #'list)
	 (definition definitions #'cons))

	(definition
	 (:type const :lparen type-args :rparen :dot #'(lambda (ty const l typs r d)
																								(declare (ignore ty l r d))
																									`(,const . ,typs))))

	(type-args
	 (atype #'list)
	 (atype :comma type-args #'(lambda (ty comma ls) (cons ty ls))))

	(atype
	 (:type-int #'(lambda (x) (declare (ignore x)) :type-int))
	 (:type-catom #'(lambda (x) (declare (ignore x)):type-catom)))

	(statements
	 (statement #'list)
	 (statement statements #'cons))
	(statement
		(terms :arrow terms :dot #'(lambda (conc y perm w) (declare (ignore y w)) (list perm '-> conc))))

	(terms
	 	(term #'list)
		(term :comma terms #'(lambda (x y z) (declare (ignore y)) (cons x z))))
	(term
	 	(const :lparen args :rparen #'(lambda (x y z w) (declare (ignore y w)) (list x z))))

	(args
	 	(arg #'list)
		(arg :comma args (lambda (x y z) (declare (ignore y)) (cons x z))))
	(arg
	 	variable
		const
		(:number #'(lambda (int) (list :int (parse-integer int)))))

	(variable
	 (:variable (lambda (x) (make-var (str->sym x)))))

	(const
	 (:const #'identity)))

(defparameter *code* "
type a(int, catom).
type b(catom, int).
type c(catom).

a(A) :- b(A,2), c(A).

c(Node) :-
	d(Node),
	e(Node).
")

(defun parse-meld (str)
 (let ((lexer (meld-lexer str)))
	(parse-with-lexer lexer meld-parser)))

