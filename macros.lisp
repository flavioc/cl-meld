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
         
(defmacro equal-or (ls &body rest)
   `(or ,@(mapcar #'(lambda (el) `(equal ',el ,ls)) rest)))

;; Meld related code

(defmacro do-definitions (code (&key (name nil) (types nil) (options nil)) &body body)
   (with-gensyms (el defs)
      `(let ((,defs (definitions ,code)))
         (dolist (,el ,defs)
            (let (,@(build-bind name `(definition-name ,el))
                  ,@(build-bind types `(definition-types ,el))
                  ,@(if options `((,options (definition-options ,el)))))
               ,@body)))))

(defmacro do-clauses (clauses (&key (head nil) (body nil) (clause nil) (id nil)) &body rest)
   (with-gensyms (el)
      `(dolist-count (,el ,clauses ,id)
         (let (,@(build-bind head `(clause-head ,el))
               ,@(build-bind body `(clause-body ,el))
               ,@(build-bind clause el))
            ,@rest))))
            
(defmacro with-subgoal (subgoal (&key (name nil) (args nil)) &body body)
   `(let (,@(build-bind name `(subgoal-name ,subgoal))
          ,@(build-bind args `(subgoal-args ,subgoal)))
      ,@body))

(defmacro do-subgoals (subgoals (&key (name nil) (args nil) (id nil) (orig nil)) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,subgoals subgoal-p ,id)
         (let (,@(if orig `((,orig ,el)) nil))
            (with-subgoal ,el (:name ,name :args ,args)
               ,@body)))))
               
(defmacro do-constraints (constraints (&key (expr nil) (orig nil) (id nil)) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,constraints constraint-p ,id)
         (let (,@(if expr `((,expr (constraint-expr ,el))) nil) ,@(if orig `((,orig ,el))))
            ,@body))))
            
(defmacro do-assignments (assignments (&key (var nil) (expr nil) (assignment nil) (id nil)) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,assignments assignment-p ,id)
         (let (,@(build-bind var `(assignment-var ,el))
               ,@(build-bind expr `(assignment-expr ,el))
               ,@(build-bind assignment `,el))
            ,@body))))
            
(defmacro do-processes (procs (&key (name nil) (instrs nil)) &body body)
   (with-gensyms (el)
      `(dolist (,el ,procs)
         (let (,@(build-bind name `(process-name ,el))
               ,@(build-bind instrs `(process-instrs ,el)))
            ,@body))))