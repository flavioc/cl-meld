
(in-package :cl-meld)

(define-string-lexer meld-lexer
   ("[-+]?[0-9]+(\\.[0-9]+|[0-9]+)?" (return (values :number $@)))
	("\\,"                           (return (values :comma $@)))
	("\\["                           (return (values :lsparen $@)))
	("\\]"                           (return (values :rsparen $@)))
	("\\{"                           (return (values :lcparen $@)))
	("\\}"                           (return (values :rcparen $@)))
   ("\\."                           (return (values :dot $@)))
   ("\\bimmediate\\b"               (return (values :immediate $@)))
 	("\\btype\\b"			            (return (values :type $@)))
 	("\\bextern\\b"                  (return (values :extern $@)))
 	("\\bconst\\b"                   (return (values :const-decl $@)))
	("\\bint\\b"			            (return (values :type-int $@)))
	("\\bfloat\\b"                   (return (values :type-float $@)))
	("\\bnode\\b"                    (return (values :type-addr $@)))
	("\\bworker\\b"                  (return (values :type-worker $@)))
	("\\blist\\b"                    (return (values :type-list $@)))
	("\\binclude\\b"                 (return (values :include $@)))
	("@world"                        (return (values :world $@)))
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
	("_"				                  (return (values :variable $@)))
 	("[a-z]([a-z]|[A-Z]|\_)*"		   (return (values :const $@)))
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
   `#'(lambda (&rest x) (declare (ignore x)) ,const))
   
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

(defvar *defined-functions* nil)
(defun add-defined-function (fun) (push fun *defined-functions*))

(defun has-function-call-p (name)
   (do-functions *defined-functions* (:name other :function f)
      (when (string-equal other name)
         (return-from has-function-call-p f)))
   nil)
   
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
(defun parser-make-definition (name types &optional option)
   (let* ((initial-options (if option (list option)))
          (all-options (append initial-options (if (may-be-worker-types-p types) '(:worker)))))
      (make-definition name types all-options)))

(defun parse-agg-construct (str)
   (cond
      ((string-equal str "count") :count)
      ((string-equal str "sum") :sum)))
      
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
 	
 	(:precedence ((:left :mul :div :mod) (:left :plus :minus)))
 	
	(:terminals (:const :type :variable :number :lparen :rparen
								:bar :arrow :dot :comma :type-int :type-addr
								:type-worker :type-float :plus :minus :mul :mod :div
								:lesser :lesser-equal :greater :greater-equal :equal
								:extern :const-decl
								:lsparen :rsparen :nil :bar :type-list :local
								:route :include :file :world :action
								:output :input :immediate :linear
								:dollar :lcparen :rcparen :lolli
								:bang :to :let :in :fun :end :colon
								:not-equal :if :then :else))

	(program
	  (includes definitions externs consts funs statements #L(make-ast  !2 ; definitions
	                                                               !3 ; externs
	                                                               (remove-if #'is-axiom-p !6) ; clauses
	                                                               (filter #'is-axiom-p !6) ; axioms
	                                                               !5
	                                                               (defined-nodes-list)))) ; nodes

	(includes
	   ()
	   (include includes #'(lambda (a b) (declare (ignore a b)))))

	(include
	   (:include :file #'(lambda (i f) (declare (ignore i)) (add-included-file f))))

	(definitions
	 ()
	 (definition definitions #'cons))

   (definition
      (:type predicate-option const type-args-part #L(parser-make-definition !3 !4 !2))
      (:type const type-args-part #L(parser-make-definition !2 !3)))

   (consts
      ()
      (const-definition consts (return-const nil)))

	(const-definition
	   (:const-decl const :equal expr :dot #'(lambda (a name e expr dot)
   	                                             (declare (ignore a e dot))
   	                                             (push (make-const-definition name expr) *parsed-consts*)
   	                                             nil)))

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
 	 (:type-worker (return-const :type-worker)))
	   
	(statements
	 ()
	 (statement statements #'cons))

	(statement
	   (terms :lolli terms :dot #'(lambda (body l head d) (declare (ignore l d)) (make-clause body head)))
	   (terms :lolli :dot #'(lambda (body l d) (declare (ignore l d)) (make-clause body nil)))
	   (:arrow terms :dot #'(lambda (x body y) (declare (ignore x y)) (make-clause body nil)))
	   (terms :dot #'(lambda (head d) (declare (ignore d)) (make-clause nil head)))
		(terms :arrow terms :dot #'(lambda (conc y perm w) (declare (ignore y w)) (make-clause perm conc))))
		
   (terms
      (term #'list)
      (term :comma terms #'(lambda (el x ls) (declare (ignore x)) (cons el ls))))
      
   (term
      (comprehension #'identity)
      (subgoal #'identity)
      (constraint #'identity)
      (aggregate-thing #'identity))

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
	                                       (make-subgoal name args))))
	 	
	(comprehension
	    (:lcparen variable-list :bar terms :bar terms :rcparen #'(lambda (l vl b1 left b2 right r) (declare (ignore l b1 b2 r))
                                              (make-comprehension left right vl))))
                                                                              
	(variable-list
	   (variable #'list)
	   (variable :comma variable-list #'(lambda (v c l) (declare (ignore c)) (cons v l))))
	
	(aggregate-thing
	   (:lsparen const :to variable :bar variable-list :bar terms :rsparen
	         #'(lambda (l aconstruct to var b1 vlist b2 terms r) (declare (ignore l r b1 b2 to))
	               (make-agg-construct (parse-agg-construct aconstruct) var vlist terms))))
	                          
   (constraint
      (cmp #'(lambda (c) (make-constraint c))))

	(args
	 	(expr #'list)
		(expr :comma args #'(lambda (x y z) (declare (ignore y)) (cons x z))))

	(expr
	   variable
	   (const :lparen args :rparen #'(lambda (name l args r) (declare (ignore l r))
	            (acond
	               ((has-function-call-p name)
	                  (generate-expression-by-function-call it args)
	                  )
	               (t (make-call name args)))))
	   (const #L(lookup-const-def !1))
	   (:local :number #L(let ((val (parse-integer !2))) (add-found-node val) (make-addr val)))
		(:number #L(parse-number !1))
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
	   
	(sub-list
	   (expr #'(lambda (expr) (make-cons expr (make-nil))))
	   (expr :comma sub-list #'(lambda (expr x sub) (declare (ignore x)) (make-cons expr sub)))
	   (expr :bar expr #'(lambda (a b c) (declare (ignore b)) (make-cons a c))))

   (cmp
      (expr :equal expr #'make-equal)
      (expr :not-equal expr #'make-not-equal)
      (expr :lesser expr #'make-lesser)
      (expr :lesser-equal expr #'make-lesser-equal)
      (expr :greater expr #'make-greater)
      (expr :greater-equal expr #'make-greater-equal))
      
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
      
(defun read-source-line (stream)
   (multiple-value-bind (line missing-newline-p) (read-line stream nil nil)
      (unless missing-newline-p
         (if (string-equal line "")
            (read-source-line stream)
            line))))

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
         (parse-with-lexer lexer meld-parser))))
   
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
   (let* ((*included-files* nil)
          (*defined-functions* nil)
          (ast (with-inner-parse-context
                  (parse-file-as-stream file))))
      (in-directory (pathname (directory-namestring (pathname file)))
         (let ((other-asts (mapcar #L(parse-meld-file-rec !1) *included-files*)))
            (reduce #'merge-asts other-asts
                     :initial-value ast)))))

(defun parse-meld-file (file)
   (with-parse-context
      (parse-meld-file-rec file)))