
(in-package :cl-meld)

(define-string-lexer meld-lexer
   ("[-+]?[0-9]+(\\.[0-9]+|[0-9]+)?" (return (values :number $@)))
	("\"[^\"]*\""							(return (values :string $@)))
	("\\,"                           (return (values :comma $@)))
	("\\["                           (return (values :lsparen $@)))
	("\\]"                           (return (values :rsparen $@)))
	("\\{"                           (return (values :lcparen $@)))
	("\\}"                           (return (values :rcparen $@)))
   ("\\."                           (return (values :dot $@)))
	("@initial"								(return (values :initial-priority $@)))
	("@asc"									(return (values :asc $@)))
	("@desc"									(return (values :desc $@)))
	("@type"									(return (values :priority-type $@)))
	("@order"								(return (values :priority-order $@)))
	("@\\+[0-9]+s"							(return (values :delay-seconds $@)))
	("@\\+[0-9]+ms"						(return (values :delay-ms $@)))
	("\\bexists\\b"						(return (values :exists $@)))
   ("\\bimmediate\\b"               (return (values :immediate $@)))
 	("\\btype\\b"			            (return (values :type $@)))
 	("\\bextern\\b"                  (return (values :extern $@)))
 	("\\bconst\\b"                   (return (values :const-decl $@)))
	("\\bint\\b"			            (return (values :type-int $@)))
	("\\bfloat\\b"                   (return (values :type-float $@)))
	("\\bnode\\b"                    (return (values :type-addr $@)))
	("\\bstring\\b"						(return (values :type-string $@)))
	("\\bworker\\b"                  (return (values :type-worker $@)))
	("\\blist\\b"                    (return (values :type-list $@)))
	("\\binclude\\b"                 (return (values :include $@)))
	("\\brandom\\b"						(return (values :random $@)))
	("@world"                        (return (values :world $@)))
	("@arg[0-9]"							(return (values :arg $@)))
	("@"                             (return (values :local $@)))
	("-o"                            (return (values :lolli $@)))
	("\\!"                           (return (values :bang $@)))
	("\\$"                           (return (values :dollar $@)))
	("\\blinear\\b"                  (return (values :linear $@)))
	("=>"                            (return (values :to $@)))
	(":-"                            (return (values :arrow  $@)))
	("\\:"                           (return (values :colon $@)))
	("\\("			                  (return (values :lparen $@)))
	("\\)"			                  (return (values :rparen $@)))
	("\\|\\|"								(return (values :or $@)))
	("\\|"                           (return (values :bar $@)))
	("\\bnil\\b"                     (return (values :nil $@)))
	("/\\*.*\\*/"                    (return (values :comment)))
	("\\<-"                          (return (values :input $@)))
	("\\->"                          (return (values :output $@)))
	("\\+"                           (return (values :plus $@)))
	("\\-"                           (return (values :minus $@)))
	("\\*"                           (return (values :mul $@)))
	("\\%"                           (return (values :mod $@)))
	("\\/"                           (return (values :div $@)))
	("\\<="                          (return (values :lesser-equal $@)))
	("\\<>"                          (return (values :not-equal $@)))
	("\\<"                           (return (values :lesser $@)))
	("\\>="                          (return (values :greater-equal $@)))
	("\\>"                           (return (values :greater $@)))
	("\\="                           (return (values :equal $@)))
	("\\broute\\b"                   (return (values :route $@)))
	("\\baction\\b"                  (return (values :action $@)))
	("\\blet\\b"                     (return (values :let $@)))
	("\\bin\\b"                      (return (values :in $@)))
	("\\bend\\b"                     (return (values :end $@)))
	("\\bfun\\b"                     (return (values :fun $@)))
	("\\bif\\b"                      (return (values :if $@)))
   ("\\bthen\\b"                    (return (values :then $@)))
   ("\\belse\\b"                    (return (values :else $@)))
	("\\bpriority\\b"							(return (values :prio $@)))
	("\\bmin\\b"							(return (values :min $@)))
	("_"				                  (return (values :variable $@)))
 	("[a-z]([a-z]|[A-Z]|[0-9]|\\-|\_|\\?|\\-)*"		   (return (values :const $@)))
	("[A-Z]([A-Z]|\_)+"					(return (values :const $@)))
	("'\\w+"		                     (return (values :const $@)))
	("\\#.+"                         (return (values :file $@)))
	("[A-Z]([a-z]|[0-9]|[A-Z]|\_)*"	   	(return (values :variable $@))))

(defun make-var-parser (var)
   (if (equal var "_")
	   (generate-random-var)
	   (make-var var)))
	
(defun parse-base-number (str)
	(if (find #\. str)
		(read-from-string str)
		(parse-integer str)))

(defun parse-number (str)
   (if (find #\. str)
      (make-float (read-from-string str))
      (make-int (parse-integer str))))

(defun parse-delay-seconds (str)
	(let* ((remain1 (subseq str 2))
			 (remain (subseq remain1 0 (1- (length remain1)))))
		(* 1000 (parse-integer remain))))
(defun parse-delay-ms (str)
	(let* ((remain1 (subseq str 2))
			 (remain (subseq remain1 0 (- (length remain1) 2))))
		(parse-integer remain)))

(defun make-const-definition (name expr) `(:const ,name ,expr))
(defun const-definition-p (const) (tagged-p const :const))
(defun const-definition-name (const) (second const))
(defun const-definition-expr (const) (third const))

(defvar *parsed-consts* nil)
(defun lookup-const-def (name)
	(let ((x (find-if #L(equal (const-definition-name !1) name) *parsed-consts*)))
		(assert x)
		(const-definition-expr x)))
(defun has-const-def-p (name)
	(find-if #L(string-equal (const-definition-name !1) name) *parsed-consts*))
   
(defmacro return-const (const)
   `#'(lambda (&rest x) (declare (ignore x)) ,const))
   
(defvar *found-nodes* nil)

(defun add-found-node (i)
   (multiple-value-bind (yes found-p) (gethash i *found-nodes*)
      (declare (ignore yes))
      (unless found-p
         (setf (gethash i *found-nodes*) t))))
         
(defun defined-nodes-list ()
   (loop for k being the hash-keys in *found-nodes* collect k))

(defvar *included-asts* nil)
(defun add-included-ast (ast) (push ast *included-asts*))

(defvar *defined-functions* nil)
(defun add-defined-function (fun) (push fun *defined-functions*))

(defun has-function-call-p (name)
   (do-functions *defined-functions* (:name other :function f)
      (when (string-equal other name)
         (return-from has-function-call-p f)))
   nil)

(defvar *max-arg-needed* 0)
   
(defun generate-part-expression (final-type body fun-args args)
   (let ((this-fun-arg (first fun-args))
         (this-arg (first args))
         (rest-fun-args (rest fun-args))
         (rest-args (rest args)))
      (cond
         ((var-p this-arg)
            (let ((new-body (map-one-variable-to-another body this-fun-arg this-arg)))
               (setf (var-type this-arg) (var-type this-fun-arg))
               (cond
                  ((one-elem-p fun-args) new-body)
                  (t (generate-part-expression final-type
                           new-body
                           rest-fun-args
                           rest-args)))))
         (t
            (let* ((new-var-name (generate-random-var-name))
                   (new-var (make-var new-var-name (var-type this-fun-arg)))
                   (new-body (map-one-variable-to-another body this-fun-arg new-var)))
               (cond
                  ((one-elem-p fun-args)
                     (make-let new-var
                        this-arg
                        new-body 
                        final-type))
                  (t
                     (make-let new-var
                        this-arg
                        (generate-part-expression final-type new-body rest-fun-args rest-args)
                        final-type))))))))

(defun generate-expression-by-function-call (fun args)
   (with-function fun (:args fun-args :ret-type ret-type :body body :name name)
      (unless (= (length args) (length fun-args))
         (error 'parse-error :text (tostring "function call to ~a has invalid number of arguments" name)))
      (generate-part-expression ret-type body fun-args args)
      ))

(defun may-be-worker-types-p (types)
   (and types
        (type-worker-p (first types))))
(defun parser-make-definition (name types &optional options)
	(make-definition name types options))

(defun parse-agg-construct (str)
   (cond
      ((string-equal str "count") :count)
      ((string-equal str "sum") :sum)
		((string-equal str "collect") :collect)
		((string-equal str "min") :min)))
      
(defun parse-agg-decl (str)
   (cond
      ((string-equal str "sum") :sum)
      ((string-equal str "min") :min)
      ((string-equal str "max") :max)
      ((string-equal str "first") :first)
      (t
         (error 'parse-error :text (tostring "aggregate declaration not recognized ~a" str)))))

(define-parser meld-parser
 	(:start-symbol program)
 	
 	(:precedence ((:left :mul :div :mod) (:left :or :plus :minus)))
 	
	(:terminals (:const :type :variable :number :string :lparen :rparen
								:bar :arrow :dot :comma :type-int :type-addr
								:type-worker :type-float :type-string :plus :minus :mul :mod :div
								:lesser :lesser-equal :greater :greater-equal :equal
								:extern :const-decl :arg
								:lsparen :rsparen :nil :bar :type-list :local
								:route :include :file :world :action
								:output :input :immediate :linear
								:dollar :lcparen :rcparen :lolli
								:bang :to :let :in :fun :end :colon
								:not-equal :if :then :else :prio :random
								:min :asc :desc :or
								:exists :initial-priority :priority-type :priority-order
								:delay-seconds :delay-ms))

	(program
	  (includes definitions priorities externs consts funs statements #L(make-ast  !2 ; definitions
	                                                               !4 ; externs
	                                                               (remove-if #'is-axiom-p !7) ; clauses
	                                                               (filter #'is-axiom-p !7) ; axioms
	                                                               !6 ; functions
	                                                               (defined-nodes-list) ; nodes
																						!3 ; priorities
																						!5 ; consts
																						*max-arg-needed*))) ;; args-needed

	(includes
	   ()
	   (include includes #'(lambda (a b) (declare (ignore a b)))))

	(include
	   (:include :file #'(lambda (i f)
									(declare (ignore i))
									(let* ((filename (subseq f 1))
										    (inner-ast (parse-meld-file-rec filename)))
										(add-included-ast inner-ast)))))

	(definitions
	 ()
	 (definition definitions #'cons))

   (definition
      (:type predicate-options const type-args-part #L(parser-make-definition !3 !4 !2)))

	(priorities
		()
		(priority priorities #'cons))
	
	(priority
		(:prio :initial-priority :number :dot #'(lambda (p i n dot) (declare (ignore p i dot)) (make-initial-priority (parse-base-number n))))
		(:prio :priority-order asc-desc :dot #'(lambda (p o ad dot) (declare (ignore p o dot)) (make-priority-order ad)))
		(:prio const :div number asc-desc :dot #'(lambda (p name div arg ad dot) (declare (ignore p div dot)) (make-global-priority name (int-val arg) ad)))
		(:prio const :lesser const :dot #'(lambda (p name1 l name2 d) (declare (ignore p l d)) (make-descending-priority name1 name2)))
		(:prio const :greater const :dot #'(lambda (p name1 g name2 d) (declare (ignore p g d)) (make-ascending-priority name1 name2))))
		
	(asc-desc
		(:asc (return-const :asc))
		(:desc (return-const :desc)))
		
   (consts
      ()
      (const-definition consts #'cons))

	(const-definition
	   (:const-decl const-name :equal expr :dot #'(lambda (a name e expr dot)
   	                                             (declare (ignore a e dot))
   	                                             (push (make-const-definition name expr) *parsed-consts*)
																	(make-constant name expr))))

	(const-name
		const)
		
   (funs
      ()
      (fun funs #'cons))
      
   (fun
      (:fun const :lparen fun-args :rparen :colon atype :equal expr :dot
            #'(lambda (f name l args r c ret-type eq body d)
               (declare (ignore f l r c eq d))
                  (let ((fun (make-function name args ret-type body)))
                     (add-defined-function fun)
                     fun))))
                  
   (fun-args
      (fun-arg #'list)
      (fun-arg :comma fun-args #'cons))
      
   (fun-arg
      (atype variable #'(lambda (typ var) (make-var (var-name var) typ))))
         
   (externs
      ()
      (extern-definition externs #'cons))

   (extern-definition
      (:extern atype const :lparen type-args :rparen :dot #'(lambda (e ret-type name l args r d)
   	                                                         (declare (ignore e l r d))
   	                                                         (make-extern name ret-type args))))
	(predicate-options
		()
		(predicate-option predicate-options #'cons))
		
	(predicate-option
	   (:route (return-const :route))
	   (:action (return-const :action))
	   (:linear (return-const :linear)))
	   
	(type-args-part
	   (:lparen type-args :rparen :dot #'(lambda (l typs r d) (declare (ignore l r d)) typs)))

	(type-args
	 (type-decl #'list)
	 (type-decl :comma type-args #'(lambda (ty comma ls) (declare (ignore comma)) (cons ty ls))))

   (type-decl
    (atype #'identity)
    (aggregate-decl atype aggregate-mods #'make-aggregate))
    
   (aggregate-decl
		(:min (return-const :min))
      (const #L(parse-agg-decl !1)))

   (aggregate-mods
     ()
     (:lsparen :immediate :rsparen (return-const :immediate))
     (:lsparen :input const :rsparen #'(lambda (l i name r) (declare (ignore l i r)) (list :input name)))
     (:lsparen :output const :rsparen #'(lambda (l i name r) (declare (ignore l i r)) (list :output name))))

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
 	 (:type-addr (return-const :type-addr))
	 (:type-string (return-const :type-string))
 	 (:type-worker (return-const :type-worker)))
	   
	(statements
	 ()
	 (statement statements #'cons))

	(statement
		(:lsparen :colon subgoal-mod variable :bar terms :rsparen :lolli head :dot
				#'(lambda (l colon mod v b body r lolli head d)
						(declare (ignore l colon b r lolli d))
						(let ((clause (make-clause body head)))
							(case mod
								(:random (clause-add-random clause v))
								(:min (clause-add-min clause v)))
							clause)))
	   (terms :lolli head :dot #'(lambda (body l head d) (declare (ignore l d)) (make-clause body head)))
	   (terms :lolli :dot #'(lambda (body l d) (declare (ignore l d)) (make-clause body nil)))
	   (:arrow terms :dot #'(lambda (x body y) (declare (ignore x y)) (make-clause body nil)))
	   (head :dot #'(lambda (head d) (declare (ignore d)) (make-clause nil head)))
		(head :arrow terms :dot #'(lambda (conc y perm w) (declare (ignore y w)) (make-clause perm conc))))
		
	(subgoal-mod
		(:random (return-const :random))
		(:min (return-const :min)))
	
	(head
		(:number #'(lambda (str)
						(let ((num (parse-integer str)))
							(if (= num 1)
								nil
								(error 'parse-error :text (tostring "invalid head number ~a" str))))))
		(terms #'identity))
		
   (terms
      (term #'list)
      (term :comma terms #'(lambda (el x ls) (declare (ignore x)) (cons el ls))))
      
   (term
		(exists #'identity)
      (comprehension #'identity)
      (subgoal #'identity)
      (constraint #'identity)
      (aggregate-thing #'identity))

	(exists
		(:exists variable-list :dot :lparen terms :rparen #'(lambda (e var-list d l terms r)
													(declare (ignore e d l r))
													(make-exist var-list terms))))
	(subgoal
	   (inner-subgoal  #'identity)
	   (:dollar inner-subgoal #'(lambda (d sub)
	                                 (declare (ignore d))
	                                 (subgoal-add-option sub :reuse)
	                                 sub))
	 	(:bang inner-subgoal  #'(lambda (o sub)
	 	                                 (declare (ignore o))
	 	                                 (subgoal-add-option sub :persistent)
	 	                                 sub)))
	 	                                 
	 (inner-subgoal
	    (const :lparen args :rparen #'(lambda (name x args y)
	                                       (declare (ignore x y))
	                                       (make-subgoal name args)))
		 (const :lparen args :rparen tuple-delay #'(lambda (name x args y delay)
																(declare (ignore x y))
																(let ((sub (make-subgoal name args)))
																	(subgoal-add-delay sub delay)
																	sub))))
	
	(tuple-delay
		(:delay-seconds #L(parse-delay-seconds !1))
		(:delay-ms #L(parse-delay-ms !1)))
		
	(comprehension
	    (:lcparen variable-list :bar terms :bar terms :rcparen #'(lambda (l vl b1 left b2 right r) (declare (ignore l b1 b2 r))
                                              (make-comprehension left right vl))))
                                                                              
	(variable-list
		(variable #'list)
	   (variable :comma variable-list #'(lambda (v c l) (declare (ignore c)) (cons v l))))
	
	(aggregate-thing
		(:lsparen multiple-aggregate-spec :bar terms :bar terms :rsparen
			#'(lambda (l specs b1 body b2 head r)
					(declare (ignore l b1 b2 r))
					(make-agg-construct specs nil body head)))
		(:lsparen multiple-aggregate-spec :bar variable-list :bar terms :bar terms :rsparen
			#'(lambda (l specs b1 vlist b2 body b3 head r)
				(declare (ignore l r b1 b2 b3 r))
				(make-agg-construct specs vlist body head)))
	   (:lsparen multiple-aggregate-spec :bar variable-list :bar terms :rsparen
	         #'(lambda (l specs b1 vlist b2 terms r) (declare (ignore l r b1 b2))
	               (make-agg-construct specs vlist terms)))
		)
		
	(multiple-aggregate-spec
		(aggregate-spec #L(list !1))
		(aggregate-spec :comma multiple-aggregate-spec #L(cons !1 !3)))
		
	(aggregate-spec
		(aggregate-mod :to variable #L(make-agg-spec (parse-agg-construct !1) !3)))
	
	(aggregate-mod
		(:min (return-const "min"))
		(const #'identity))
	                          
   (constraint
      (cmp #'(lambda (c) (make-constraint c))))

	(args
	 	(expr #'list)
		(expr :comma args #'(lambda (x y z) (declare (ignore y)) (cons x z))))

	(expr
	   variable
		arg
		(:string #'(lambda (x) (make-string-constant (subseq x 1 (1- (length x)))))) ;; need to trim the first and final ""
	   (const :lparen args :rparen #'(lambda (name l args r) (declare (ignore l r))
	            (acond
	               ((has-function-call-p name)
	                  (generate-expression-by-function-call it args))
	               (t (make-call name args)))))
	   (const #L(if (has-const-def-p !1)
						(make-get-constant !1)
						(make-var-parser !1)))
	   (:local :number #L(let ((val (parse-integer !2))) (add-found-node val) (make-addr val)))
		number
	   (:lparen expr :rparen #'(lambda (l expr r) (declare (ignore l r)) expr))
	   (:type-float :lparen expr :rparen #'(lambda (f l expr r) (declare (ignore f l r)) (make-convert-float expr)))
	   (:world (return-const (make-world)))
	   (expr :minus expr #'make-minus)
	   (expr :mul expr #'make-mul)
	   (expr :mod expr #'make-mod)
	   (expr :div expr #'make-div)
	   (expr :plus expr #'make-plus)
	   (:if cmp :then expr :else expr :end #'(lambda (if cmp then e1 else e2 end)
	                                                (declare (ignore if then else end))
	                                                (make-if cmp e1 e2)))
	   (:let variable :equal expr :in expr :end #'(lambda (l var eq expr i body e) (declare (ignore l eq i e)) (make-let var expr body)))
	   (list-expr #'identity))

	(list-expr
	   (:lsparen sub-list :rsparen #'(lambda (a b c) (declare (ignore a c)) b))
	   (:lsparen :rsparen #'(lambda (a b) (declare (ignore a b)) (make-nil)))
	   (:nil #'(lambda (a) (declare (ignore a)) (make-nil))))
	   
	(arg
		(:arg (lambda (x)
			(let ((num-arg (parse-integer (subseq x 4 5))))
				(setf *max-arg-needed* (max num-arg *max-arg-needed*))
				(make-argument num-arg)))))

	(sub-list
	   (expr #'(lambda (expr) (make-cons expr (make-nil))))
	   (expr :comma sub-list #'(lambda (expr x sub) (declare (ignore x)) (make-cons expr sub)))
	   (expr :bar expr #'(lambda (a b c) (declare (ignore b)) (make-cons a c))))

   (cmp
		(cmp :or cmp #'make-or)
		(:lparen cmp :rparen #'(lambda (l cmp r) (declare (ignore l r)) cmp))
      (expr :equal expr #'make-equal)
      (expr :not-equal expr #'make-not-equal)
      (expr :lesser expr #'make-lesser)
      (expr :lesser-equal expr #'make-lesser-equal)
      (expr :greater expr #'make-greater)
      (expr :greater-equal expr #'make-greater-equal)
		)

	(number
		(:number #L(parse-number !1)))
      
	(variable
	 (:variable (lambda (x) (make-var-parser x))))

	(const
	 (:const #'identity)))
	 
(defmacro with-parse-context (&body body)
   `(let ((*parsed-consts* nil))
      ,@body))
      
(defmacro with-inner-parse-context (&body body)
   `(let ((*found-nodes* (make-hash-table)))
      ,@body))

(defun strip-comments-from-line (line)
	(ppcre:regex-replace "//.+" line ""))
      
(defun read-source-line (stream)
   (multiple-value-bind (line missing-newline-p) (read-line stream nil nil)
      (unless missing-newline-p
         (if (string-equal line "")
            (read-source-line stream)
            (strip-comments-from-line line)))))

(defun simple-stream-lexer (read-source-line string-lexer &key (stream *standard-input*))
  (let (eof line-lexer (update t))
    (labels ((update-line-lexer ()
               (let ((line (funcall read-source-line stream)))
                  (if (null line)
                     (setf eof t))
                  (setf line-lexer (funcall string-lexer line))))
            (get-next-token ()
               (multiple-value-bind (token value)
                     (funcall line-lexer)
                  (if token
                     (values token value)
                     (if eof
                        nil
                        (progn
                           (update-line-lexer)
                           (get-next-token)))))))
      (lambda ()
         (when eof
            (error 'end-of-file :stream stream))
         (when update
            (update-line-lexer)
            (setf update nil))
         (get-next-token)))))
         
(defun parse-file-as-stream (file)
   "Takes a stream-based lexer for the file and parses the stream."
   (unless (file-exists-p file)
      (error 'file-not-found-error :text file))
   (with-open-file (input-stream file
                        :direction :input
                        :if-does-not-exist :error)
      (let* ((lexer (simple-stream-lexer #'read-source-line
                                  #'meld-lexer
                                  :stream input-stream)))
		(in-directory (pathname (directory-namestring (pathname file)))
         (parse-with-lexer lexer meld-parser)))))
   
(defun parse-string (str)
   "Parses a string of Meld code."
   (let* ((lexer (meld-lexer str)))
      (parse-with-lexer lexer meld-parser)))
      
(defun parse-file (file)
   "Parses a file of Meld code."
   (unless (file-exists-p file)
      (error 'file-not-found-error :text file))
   (parse-string (read-file file)))
             
(define-condition file-not-found-error (error)
   ((text :initarg :text :reader text)))

(defun parse-meld-file-rec (file)
   "Parses a Meld file, including included files."
   (let* ((*included-asts* nil)
          (*defined-functions* nil)
			 (*max-arg-needed* 0)
          (ast (with-inner-parse-context
                  (parse-file-as-stream file))))
       (reduce #'merge-asts *included-asts*
                  :initial-value ast)))

(defun parse-meld-file (file)
   (with-parse-context
      (parse-meld-file-rec file)))
