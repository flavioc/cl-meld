
(in-package :cl-meld)

(define-string-lexer meld-lexer
 	("type"			(return (values :type $@)))
 	("extern"      (return (values :extern $@)))
 	("const"       (return (values :const-decl $@)))
	("int"			(return (values :type-int $@)))
	("float"       (return (values :type-float $@)))
	("node"        (return (values :type-addr $@)))
	("list"        (return (values :type-list $@)))
	("include"     (return (values :include $@)))
	(":-"          (return (values :arrow  $@)))
	("\\("			(return (values :lparen $@)))
	("\\)"			(return (values :rparen $@)))
	("\\["         (return (values :lsparen $@)))
	("\\]"         (return (values :rsparen $@)))
	("\\|"         (return (values :bar $@)))
	("nil"         (return (values :nil $@)))
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
	("route"       (return (values :route $@)))
	("@"           (return (values :local $@)))
	("[-+]?[0-9]+(\.[0-9]+|[0-9]+)?" (return (values :number $@)))
	("_"				                  (return (values :variable $@)))
 	("[a-z]([a-z]|[A-Z]|\_)*"		         (return (values :const $@)))
	("'\\w+"		                     (return (values :const $@)))
	("\\#.+"                         (return (values :file $@)))
	("[A-Z]([A-Z]|[a-z]|[0-9])*"	   (return (values :variable $@))))

(defun make-var-parser (var)
   (if (equal var "_")
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

(defvar *parsed-consts* nil)
(defun lookup-const-def (name)
   (const-definition-expr (find-if #L(equal (const-definition-name !1) name) *parsed-consts*)))
   
(defmacro return-const (const)
   `#'(lambda (x) (declare (ignore x)) ,const))
   
(defvar *found-nodes* nil)
(defun add-found-node (i)
   (multiple-value-bind (yes found-p) (gethash i *found-nodes*)
      (declare (ignore yes))
      (unless found-p
         (setf (gethash i *found-nodes*) t))))
(defun defined-nodes-list ()
   (loop for k being the hash-keys in *found-nodes* collect k))
   
(defvar *included-files* nil)
(defun add-included-file (file) (push (subseq file 1) *included-files*))

(define-parser meld-parser
   (:muffle-conflicts t)
 	(:start-symbol program)
	(:terminals (:const :type :variable :number :lparen :rparen
								:bar :arrow :dot :comma :type-int :type-addr
								:type-float :plus :minus :mul :mod :div
								:lesser :lesser-equal :greater :greater-equal :equal
								:extern :const-decl :min :max :first :sum
								:lsparen :rsparen :nil :bar :type-list :local
								:route :include :file))
	(program
	  (includes definitions statements #L(make-ast !2 !3)))
	  
	(includes
	   ()
	   (include includes #'(lambda (a b) (declare (ignore a b)))))
	   
	(include
	   (:include :file #'(lambda (i f) (declare (ignore i)) (add-included-file f))))

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
	 (predicate-definition #'identity))
	 
	
	(predicate-definition
	 (:type predicate-option const type-args-part #L(make-definition !3 !4 !2))
	 (:type const type-args-part #L(make-definition !2 !3)))
	 							
	(predicate-option
	   (:route (return-const :route)))
	   
	(type-args-part
	   (:lparen type-args :rparen :dot #'(lambda (l typs r d) (declare (ignore l r d)) typs)))

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
	 base-type
	 (:type-list base-type #'(lambda (l ty)
	                           (declare (ignore l))
	                           (case ty
	                              (:type-int :type-list-int)
	                              (:type-float :type-list-float)
	                              (:type-addr :type-list-addr)))))
	 
	(base-type
 	 (:type-int (return-const :type-int))
 	 (:type-float (return-const :type-float))
 	 (:type-addr (return-const :type-addr)))
	   

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
	   (:local :number #L(let ((val (parse-integer !2))) (add-found-node val) (make-addr val)))
		(:number #L(parse-number !1))
	   (:lparen expr :rparen #'(lambda (l expr r) (declare (ignore l r)) expr))
	   (:type-float :lparen expr :rparen #'(lambda (f l expr r) (declare (ignore f l r)) (make-convert-float expr)))
	   (expr :minus expr #'make-minus)
	   (expr :mul expr #'make-mul)
	   (expr :mod expr #'make-mod)
	   (expr :div expr #'make-div)
	   (expr :plus expr #'make-plus)
	   (list-expr #'identity))
	   
	(list-expr
	   (:lsparen sub-list :rsparen #'(lambda (a b c) (declare (ignore a c)) b))
	   (:lsparen :rsparen #'(lambda (a b) (declare (ignore a b)) (make-nil)))
	   (:nil #'(lambda (a) (declare (ignore a)) (make-nil))))
	   
	(sub-list
	   (expr #'(lambda (expr) (make-cons expr (make-nil))))
	   (expr :comma sub-list #'(lambda (expr x sub) (declare (ignore x)) (make-cons expr sub)))
	   (expr :bar expr #'(lambda (a b c) (declare (ignore b)) (make-cons a c))))

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
        (*parsed-consts* nil)
        (*found-nodes* (make-hash-table))
        (result (parse-with-lexer lexer meld-parser)))
   (make-ast (all-definitions result)
             (clauses result)
             (defined-nodes-list))))
             
(defun read-file (file)
   (with-open-file (str file
                        :direction :input
                        :if-does-not-exist :error)
      (reduce #L(concatenate 'string !1 !2 (list #\newline))
         (loop for line = (read-line str nil nil)
                while line
                collect line) :initial-value "")))

(defun parse-meld-file (file)
   (let* ((pn (pathname file))
          (old-directory *default-pathname-defaults*)
          (*included-files* nil)
          (str (read-file file)))
      (setf *default-pathname-defaults* (pathname (directory-namestring pn)))
      (let ((ast (parse-meld str)))
         (loop for aux-file in *included-files*
               do (let ((ast-aux (parse-meld-file aux-file)))
                     (setf ast (make-ast (append (all-definitions ast-aux) (all-definitions ast))
                                         (append (clauses ast-aux) (clauses ast))
                                         (append (defined-nodes ast-aux) (defined-nodes ast))))))
         (setf *default-pathname-defaults* old-directory)
         ast)))