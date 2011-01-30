
(in-package :cl-meld)

(define-string-lexer meld-lexer
 	("type"			(return (values :type $@)))
	("int"			(return (values :type-int $@)))
	("float"       (return (values :type-float $@)))
	("catom"		(return (values :type-catom $@)))
	("node"     (return (values :type-node $@)))
	(":-"           (return (values :arrow  $@)))
	("\\("			(return (values :lparen $@)))
	("\\)"			(return (values :rparen $@)))
	("\\."          (return (values :dot $@)))
	("\\,"          (return (values :comma $@)))
	("\\+"          (return (values :plus $@)))
	("\\-"          (return (values :minus $@)))
	("\\*"          (return (values :mul $@)))
	("\\%"          (return (values :mod $@)))
	("\\/"          (return (values :div $@)))
	("\\<="         (return (values :lesser-equal $@)))
	("\\<"          (return (values :lesser $@)))
	("\\>"          (return (values :greater $@)))
	("\\>="         (return (values :greater-equal $@)))
	("\\=="         (return (values :equal $@)))
	("\\d+"		(return (values :number $@)))
	("_"				(return (values :variable $@)))
 	("[a-z]+"		(return (values :const $@)))
	("'\\w+'"		(return (values :const $@)))
	("[A-Z]([A-Z]|[a-z])*"	(return (values :variable $@))))

(defun str->sym (str) (values (intern str)))

(defun make-var (var)
 (if (eq var '_)
	:placeholder
	`(:var ,var)))
	
(defun make-int (int) (list :int (parse-integer int)))
(defun make-plus (e1 p e2) (list :plus e1 e2))
(defun make-minus (e1 m e2) (list :minus e1 e2))
(defun make-mul (e1 m e2) (list :mul e1 e2))
(defun make-mod (e1 m e2) (list :mod e1 e2))
(defun make-div (e1 d e2) (list :div e1 e2))
(defun make-lesser (e1 l e2) (list :lesser e1 e2))
(defun make-lesser-equal (e1 l e2) (list :lesser-equal e1 e2))
(defun make-greater (e1 g e2) (list :greater e1 e2))
(defun make-greater-equal (e1 g e2) (list :greater-equal e1 e2))
(defun make-equal (e1 e e2) (list :equal e1 e2))
			
(define-parser meld-parser
 	(:start-symbol program)
	(:terminals (:const :type :variable :number :lparen :rparen
								:bar :arrow :dot :comma :type-int :type-node
								:type-catom :type-float :plus :minus :mul :mod :div
								:lesser :lesser-equal :greater :greater-equal :equal))
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
	 (:type-float #'(lambda (x) (declare (ignore x)) :type-float))
	 (:type-catom #'(lambda (x) (declare (ignore x)) :type-node))
	 (:type-node #'(lambda (x) (declare (ignore x)) :type-node)))

	(statements
	 (statement #'list)
	 (statement statements #'cons))
	 
	(statement
		(head-terms :arrow body-terms :dot #'(lambda (conc y perm w) (declare (ignore y w)) (list perm '-> conc))))

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
	 	(const :lparen args :rparen #'(lambda (x y z w) (declare (ignore y w)) (list x z))))

   (constraint
      (cmp #'(lambda (c) (list :constraint c))))
      
	(args
	 	(expr #'list)
		(expr :comma args (lambda (x y z) (declare (ignore y)) (cons x z))))

	(expr
	   variable
		const
		(:number #'make-int)
	   (:lparen expr :rparen #'(lambda (l expr r) expr))
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
	 (:variable (lambda (x) (make-var (str->sym x)))))

	(const
	 (:const #'identity)))

(defun parse-meld (str)
 (let ((lexer (meld-lexer str)))
	(parse-with-lexer lexer meld-parser)))

(defparameter *code* "
type a(node, catom).
type b(node, float).
type c(node).
type d(node).
type e(node).

a(A, A) :- b(A,((B-2)+3)+1), d(A), 2 < 3.

c(Node) :-
	d(Node),
	2 == 2,
	e(Node).
")

(defparameter *parsed* (parse-meld *code*))