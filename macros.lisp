(in-package :cl-meld)

;; Several macro utilities

(defmacro mac (expr)
 `(pprint (macroexpand-1 ',expr)))
 
(defmacro ensure-bool (form) `(if ,form t nil))
 
(defmacro on-top-level (&rest forms)
   `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@forms))
         
(defmacro with-var (var &body body)
   `(let (,var)
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

(defmacro do-definitions (code (name types &optional options) &body body)
   (with-gensyms (el defs)
      `(let ((,defs (definitions ,code)))
         (dolist (,el ,defs)
            (let ((,name (definition-name ,el))
                  (,types (definition-types ,el))
                  ,@(if options `((,options (definition-options ,el)))))
               ,@body)))))

(defmacro do-clauses (code (head body &optional id) &body rest)
   (with-gensyms (el clauses)
      `(let ((,clauses (clauses ,code))
             ,@(if id `((,id 0))))
         (dolist (,el ,clauses)
            (let ((,head (clause-head ,el))
                  (,body (clause-body ,el)))
               ,@(if id `((incf ,id)))
               ,@rest)))))

(defmacro do-subgoals (subgoals (name args &optional id) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,subgoals subgoal-p ,id)
            (let ((,name (subgoal-name ,el))
                  (,args (subgoal-args ,el)))
               ,@body))))
               
(defmacro do-constraints (constraints (expr &optional orig id) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,constraints constraint-p ,id)
         (let ((,expr (constraint-expr ,el)) ,@(if orig `((,orig ,el))))
            ,@body))))
            
(defmacro do-assignments (assignments (var expr &optional id) &body body)
   (with-gensyms (el)
      `(dolist-filter (,el ,assignments assignment-p ,id)
         (let ((,var (assignment-var ,el))
               (,expr (assignment-expr ,el)))
            ,@body))))