
;; Several macro utilities

(defmacro mac (expr)
 `(pprint (macroexpand-1 ',expr)))
 
(defmacro with-gensyms (syms &body body)
   `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                  syms)
      ,@body))

;; Meld related code

(defmacro do-definitions (code (name types) &body body)
   (with-gensyms (el defs)
      `(let ((,defs (definitions ,code)))
         (dolist (,el ,defs)
            (let ((,name (first ,el))
                  (,types (rest ,el)))
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
               
(defmacro do-subgoals (subgoals (name args) &body body)
   (with-gensyms (el)
      `(dolist (,el ,subgoals)
         (let ((,name (subgoal-name ,el))
               (,args (subgoal-args ,el)))
            ,@body))))
            
(defmacro do-args (args (typ val &optional id) &body body)
   (with-gensyms (el)
      `(let (,@(if id `((,id 0))))
         (dolist (,el ,args)
            (let ((,typ (arg-type ,el))
                  (,val (arg-val ,el)))
               ,@(if id `((incf ,id)))
               ,@body)))))