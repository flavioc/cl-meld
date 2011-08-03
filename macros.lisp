(in-package :cl-meld)

(defun build-bind (var body)
   (if var `((,var ,body))))

;; Several macro utilities

(defmacro mac (expr)
 `(pprint (macroexpand-1 ',expr)))
 
(defmacro tostring (&rest args)
   `(with-output-to-string (str)
      (format str ,@args)))
         
(defmacro ensure-bool (form) `(if ,form t nil))
 
(defmacro on-top-level (&rest forms)
   `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@forms))
         
(defmacro with-var (var &body body)
   `(let (,var)
      ,@body
      ,var))
      
(defmacro with-ret (var &body body)
   `(with-var ,var
      ,@body
      ,var))
      
(defmacro letret ((var form) &body body)
   `(let ((,var ,form))
      ,@body
      ,var))

(defmacro always-ret (form &body body)
   (with-gensyms (ret)
      `(letret (,ret ,form)
         ,@body)))
      
(defmacro iff (test thing)
   (with-gensyms (ret)
      `(let ((,ret ,thing))
         (when (,test ,ret) ,ret))))
      
(defmacro dolist2 ((el1 ls1) (el2 ls2) &body body)
   `(loop for ,el1 in ,ls1
          for ,el2 in ,ls2
          do ,@body))
          
(defmacro with-optional-counter (id &body body)
   (if (null id)
      body
      `(let ((,id 0))
         ,@body
         (incf ,id))))
      
(defmacro dolist-filter ((el list filter &optional id) &body body)
   `(let (,@(if id `((,id 0))))
      (dolist (,el ,list)
         (when (,filter ,el)
            ,@(if id `((incf ,id)))
            ,@body))))
         
(defmacro dolist-count ((el list &optional id) &body body)
   `(let (,@(if id `((,id 0))))
      (dolist (,el ,list)
         ,@(if id `((incf ,id)))
         ,@body)))
         
(defmacro loop-list ((el list &key (id nil) (operation 'do)) &body body)
   `(loop for ,el in ,list
          ,@(when id `(for ,id upto (length ,list)))
         ,operation ,@body))
         
(defmacro with-symbol ((symb expr) &body body)
   `(symbol-macrolet ((,symb ,expr))
      ,@body))
      
(defmacro with-car ((cr cons) &body body)
   `(with-symbol (,cr (car ,cons))
      ,@body))
      
(defmacro with-cdr ((cr cons) &body body)
   `(with-symbol (,cr (cdr ,cons))
      ,@body))
         
(defmacro loop-cons ((el list) &body body)
   `(loop for ,el on ,list
          do (progn ,@body)))
         
(defmacro loop-cons-car ((el-car list) &body body)
   (with-gensyms (el)
      `(loop-cons (,el ,list)
         (with-car (,el-car ,el)
            ,@body))))
      
(defmacro equal-or (ls &body rest)
   `(or ,@(mapcar #'(lambda (el) `(equal ',el ,ls)) rest)))
   
(defmacro iterate-hash ((hash key val &key (op 'do)) &body body)
   `(loop for ,key being the hash-keys of ,hash
          using (hash-value ,val)
          ,op ,@body))
          
(defmacro in-directory (new-dir &body body)
   "Executes a piece of code inside directory 'new-dir' and goes back to the initial directory."
   (with-gensyms (old-dir)
      `(let ((,old-dir *default-pathname-defaults*))
         (unwind-protect
            (progn
               (setf *default-pathname-defaults* ,new-dir)
               ,@body)
            (setf *default-pathname-defaults* ,old-dir)))))

;; Meld related code

(defmacro with-definition (def (&key (name nil) (types nil) (options nil) (definition nil) (num-args nil)) &body body)
   `(let (,@(build-bind name `(definition-name ,def))
          ,@(build-bind types `(definition-types ,def))
          ,@(build-bind options `(definition-options ,def))
          ,@(build-bind definition def)
          ,@(build-bind num-args `(length (definition-types ,def))))
      ,@body))
      
(defmacro do-definitions-list (ls (&key definition name types options id (operation 'do)) &body body)
   (with-gensyms (el)
      `(loop-list (,el ,ls :id ,id :operation ,operation)
         (with-definition ,el (:name ,name :types ,types :options ,options :definition ,definition)
            ,@body))))
      
(defmacro do-definitions ((&key definition name types options id (operation 'do)) &body body)
   `(do-definitions-list *definitions* (:name ,name :types ,types :options ,options :definition ,definition
                                       :id ,id :operation ,operation)
      ,@body))
            
(defmacro par-collect-definitions ((&key definition name types options) &body body)
   (with-gensyms (el)
      `(par-mapcar #'(lambda (,el)
                        (with-definition ,el (:name ,name :types ,types :options ,options :definition ,definition)
                           ,@body))
                  *definitions*)))
                   
(defmacro do-node-definitions ((&key definition name types options id (operation 'do))
                                &body body)
   `(do-definitions-list *node-definitions* (:name ,name :types ,types :options ,options :definition ,definition
                                       :operation ,operation :id ,id)
      ,@body))

(defmacro do-worker-definitions ((&key definition name types options (operation 'do))
                                &body body)
   `(do-definitions-list *worker-definitions* (:name ,name :types ,types :options ,options :definition ,definition
                                       :operation ,operation)
      ,@body))
         
(defmacro with-extern (extern (&key (name nil) (ret-type nil) (types nil)) &body body)
   `(let (,@(build-bind name `(extern-name ,extern))
          ,@(build-bind ret-type `(extern-ret-type ,extern))
          ,@(build-bind types `(extern-types ,extern)))
      ,@body))
      
(defmacro do-externs (code (&key (name nil) (ret-type nil) (types nil) (id nil) (operation 'do)) &body body)
   (with-gensyms (el)
      `(loop-list (,el (externs ,code) :id ,id :operation ,operation)
         (with-extern ,el (:name ,name :ret-type ,ret-type :types ,types)
            ,@body))))
              
(defmacro with-clause (clause (&key (head nil) (body nil) (options nil)) &body rest)
   `(let (,@(build-bind head `(clause-head ,clause))
          ,@(build-bind body `(clause-body ,clause))
          ,@(build-bind options `(clause-options ,clause)))
      ,@rest))

(defmacro do-clauses (clauses (&key head body clause options id (operation 'do)) &body rest)
   (with-gensyms (el)
      `(loop-list (,el ,clauses :id ,id :operation ,operation)
         (let (,@(build-bind clause el))
            (with-clause ,el (:head ,head :body ,body :options ,options)
               ,@rest)))))
               
(defmacro par-do-clauses (clauses (&key (head nil) (body nil) (clause nil)
                                    (options nil)) &body rest)
   (with-gensyms (el)
      `(par-dolist (,el ,clauses)
         (let (,@(build-bind clause el))
            (with-clause ,el (:head ,head :body ,body :options ,options)
               ,@rest)))))
               
(defmacro do-rules ((&key head body clause options id (operation 'do)) &body rest)
   `(do-clauses *clauses* (:head ,head :body ,body :clause ,clause
                           :options ,options :id ,id :operation ,operation)
      ,@rest))

(defmacro do-worker-rules ((&key head body clause options id (operation 'do)) &body rest)
   `(do-clauses *worker-clauses* (:head ,head :body ,body :clause ,clause
                           :options ,options :id ,id :operation ,operation)
      ,@rest))
      
(defmacro do-all-rules ((&key head body clause options) &body rest)
   `(progn
      (do-rules (:head ,head :body ,body :clause ,clause :options ,options)
         ,@rest)
      (do-worker-rules (:head ,head :body ,body :clause ,clause :options ,options)
         ,@rest)))
         
(defmacro par-do-rules ((&key head body clause options) &body rest)
   `(par-do-clauses *clauses* (:head ,head :body ,body :clause ,clause :options ,options)
      ,@rest))
      
(defmacro do-axioms ((&key head body clause options id (operation 'do)) &body rest)
   `(do-clauses *axioms* (:head ,head :body ,body :clause ,clause
                           :options ,options :id ,id :operation ,operation)
      ,@rest))
      
(defmacro do-worker-axioms ((&key head body clause options id (operation 'do)) &body rest)
   `(do-clauses *worker-axioms* (:head ,head :body ,body :clause ,clause
                           :options ,options :id ,id :operation ,operation)
      ,@rest))
      
(defmacro par-do-axioms ((&key head body clause options) &body rest)
   `(par-do-clauses *axioms* (:head ,head :body ,body :clause ,clause :options ,options)
      ,@rest))
      
(defmacro do-all-axioms ((&key head body clause options) &body rest)
   `(progn
      (do-axioms (:head ,head :body ,body :clause ,clause :options ,options)
         ,@rest)
      (do-worker-axioms (:head ,head :body ,body :clause ,clause :options ,options)
         ,@rest)))
            
(defmacro with-subgoal (subgoal (&key name args) &body body)
   `(let (,@(build-bind name `(subgoal-name ,subgoal))
          ,@(build-bind args `(subgoal-args ,subgoal)))
      ,@body))
      
(defmacro do-subgoal-list (ls (&key (name nil) (args nil) (id nil)
                                    (subgoal nil) (operation 'do))
                               &body body)
   (with-gensyms (el)
      `(loop-list (,el ,ls :id ,id :operation ,operation)
         (let (,@(build-bind subgoal el))
            (when (subgoal-p ,el)
               (with-subgoal ,el (:name ,name :args ,args)
                  ,@body))))))

(defmacro do-subgoals (subgoals (&key (name nil) (args nil) (id nil)
                                      (subgoal nil) (operation 'do))
                                 &body body)
   (let ((arg-list `(:name ,name :args ,args :id ,id
                     :subgoal ,subgoal :operation ,operation)))
      `(cond
         ((clause-p ,subgoals)
            (do-subgoal-list (clause-body ,subgoals) ,arg-list ,@body)
            (do-subgoal-list (clause-head ,subgoals) ,arg-list ,@body))
         (t (do-subgoal-list ,subgoals ,arg-list ,@body)))))

(defmacro do-constraints (constraints (&key (expr nil) (constraint nil) (id nil)) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,constraints constraint-p ,id)
         (let (,@(build-bind expr `(constraint-expr ,el))
               ,@(build-bind constraint el))
            ,@body))))
            
(defmacro do-assignments (assignments (&key (var nil) (expr nil) (assignment nil) (id nil)) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,assignments assignment-p ,id)
         (let (,@(build-bind var `(assignment-var ,el))
               ,@(build-bind expr `(assignment-expr ,el))
               ,@(build-bind assignment `,el))
            ,@body))))
            
(defmacro with-process (process (&key (name nil) (instrs nil) (proc nil)) &body body)
   (with-gensyms (el)
      `(let ((,el ,process))
         (let (,@(build-bind name `(process-name ,el))
               ,@(build-bind proc el)
               ,@(build-bind instrs `(process-instrs ,el)))
            ,@body))))
            
(defmacro do-processes ((&key (proc nil) (name nil) (instrs nil) (operation 'do)) &body body)
   (with-gensyms (el)
      `(loop-list (,el *code* :operation ,operation)
         (with-process ,el (:name ,name :instrs ,instrs :proc ,proc)
            ,@body))))
            