(in-package :cl-meld)

(define-condition compile-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun alloc-reg (regs data)
   (let ((reg (make-reg (length regs))))
      (values reg (cons data regs))))

(defparameter *vars-places* nil)
(defparameter *used-regs* nil)
(defmacro let-compile (&body body)
   `(let ((*vars-places* (make-hash-table))
          (*used-regs* nil))
      ,@body))

(defun alloc-new-reg (data) (alloc-reg *used-regs* data))
(defmacro with-reg ((reg &optional data) &body body)
   `(multiple-value-bind (,reg *used-regs*) (alloc-new-reg ,data)
      ,@body))
      
(defun lookup-used-var (var-name)
   (multiple-value-bind (data found) (gethash var-name *vars-places*)
      (when found data)))
(defun add-used-var (var-name data) (setf (gethash var-name *vars-places*) data))
(defun remove-used-var (var-name) (remhash var-name *vars-places*))
(defun all-used-var-names () (hash-table-keys *vars-places*))

(defun valid-constraint-p (all-vars) #L(subsetp (all-variable-names !1) all-vars))
(defun get-compile-constraints-and-assignments (body)
   (let* ((assignments (select-valid-assignments body nil (all-used-var-names)))
          (vars-ass (mapcar #L(var-name (assignment-var !1)) assignments))
          (all-vars (append vars-ass (all-used-var-names)))
          (constraints (filter (valid-constraint-p all-vars) (get-constraints body)))
          (remain (remove-unneeded-assignments (append assignments constraints))))
      (split-mult-return #'constraint-p remain)))

(defun make-low-constraint (typ v1 v2) `(,typ ,v1 ,v2))
(defun low-constraint-type (lc) (first lc))
(defun low-constraint-v1 (lc) (second lc))
(defun low-constraint-v2 (lc) (third lc))

(defmacro return-expr (place &optional code) `(values ,place ,code *used-regs*))

(defmacro with-compiled-expr ((place code) expr &body body)
   `(multiple-value-bind (,place ,code *used-regs*) (compile-expr ,expr) ,@body))
   
(defmacro with-dest-or-new-reg ((dest) &body body)
   `(if (null ,dest)
      (with-reg (,dest) ,@body)
      (progn ,@body)))

(defun build-special-move (from to)
   (cond
      ((vm-nil-p from) (make-move-nil to))
      (t (make-move from to))))

(defmacro compile-expr-to (expr place)
   (with-gensyms (new-place code)
      `(multiple-value-bind (,new-place ,code *used-regs*) (compile-expr ,expr ,place)
         (if (not (equal ,new-place ,place))
            (append ,code (list (build-special-move ,new-place ,place)))
            ,code))))

(defun compile-call (name args regs code)
   (if (null args)
      (with-reg (new-reg)
         (let ((new-code `(,@code ,(make-vm-call name new-reg regs))))
            (return-expr new-reg new-code)))
      (with-compiled-expr (arg-place arg-code) (first args)
         (multiple-value-bind (place code *used-regs*)
            (compile-call name (rest args) `(,@regs ,arg-place) `(,@code ,@arg-code))
            (return-expr place code)))))
         
(defun compile-expr (expr &optional dest)
   (cond
      ((int-p expr) ;; Int expression in the previous phases maybe coerced into a float
         (let ((typ (expr-type expr)))
            (return-expr (funcall (if (type-int-p typ) #'make-vm-int #'make-vm-float)
                              (int-val expr)))))
      ((float-p expr) (return-expr (make-vm-float (float-val expr))))
      ((addr-p expr) (return-expr (make-vm-addr (addr-num expr))))
      ((host-id-p expr) (return-expr (make-vm-host-id)))
      ((var-p expr) (return-expr (lookup-used-var (var-name expr))))
      ((call-p expr)
         (compile-call (call-name expr) (call-args expr) nil nil))
      ((convert-float-p expr)
         (with-compiled-expr (place code) (convert-float-expr expr)
            (with-dest-or-new-reg (dest)
               (return-expr dest `(,@code ,(make-vm-convert-float place dest))))))
      ((tail-p expr) (with-compiled-expr (place code) (tail-list expr)
                        (with-dest-or-new-reg (dest)
                           (return-expr dest `(,@code ,(make-vm-tail place dest (expr-type expr)))))))
      ((head-p expr) (with-compiled-expr (place code) (head-list expr)
                        (with-dest-or-new-reg (dest)
                           (let ((typ (expr-type (vm-head-cons expr))))
                              (return-expr dest `(,@code ,(make-vm-head place dest typ)))))))
      ((cons-p expr) (with-compiled-expr (place-head code-head) (cons-head expr)
                        (with-compiled-expr (place-tail code-tail) (cons-tail expr)
                           (with-dest-or-new-reg (dest)
                              (let ((cons (make-vm-cons place-head place-tail dest (expr-type expr))))
                                 (return-expr dest `(,@code-head ,@code-tail ,cons)))))))
      ((not-p expr) (with-compiled-expr (place-expr code-expr) (not-expr expr)
                        (with-dest-or-new-reg (dest)
                           (return-expr dest `(,@code-expr ,(make-vm-not place-expr dest))))))
      ((test-nil-p expr) (with-compiled-expr (place-expr code-expr) (test-nil-expr expr)
                           (with-dest-or-new-reg (dest)
                              (return-expr dest `(,@code-expr ,(make-vm-test-nil place-expr dest))))))
      ((nil-p expr) (return-expr (make-vm-nil)))
      ((world-p expr) (return-expr (make-vm-world)))
      ((colocated-p expr)
         (with-compiled-expr (first-place first-code) (colocated-first expr)
            (with-compiled-expr (second-place second-code) (colocated-second expr)
               (with-dest-or-new-reg (dest)
                  (return-expr dest `(,@first-code ,@second-code ,(make-vm-colocated first-place second-place dest)))))))
      ((op-p expr)
         (with-compiled-expr (place1 code1) (op-op1 expr)
            (with-compiled-expr (place2 code2) (op-op2 expr)
               (with-dest-or-new-reg (dest)
                  (return-expr dest (generate-op-instr expr dest place1 place2 code1 code2))))))
      (t (error 'compile-invalid-error :text (tostring "Unknown expression to compile: ~a" expr)))))
      
(defun generate-op-instr (expr dest place1 place2 code1 code2)
   (let* ((base-op (op-op expr))
          (op1 (op-op1 expr)) (op2 (op-op2 expr))
          (ret-type (expr-type expr)) (operand-type (expr-type op1)))
      (cond
         ((and (equal-p expr) (or (nil-p op1) (nil-p op2)))
            (let ((place (if (nil-p op1) place2 place1))
                  (code (if (nil-p op1) code2 code1)))
               `(,@code ,(make-vm-test-nil place dest))))
         (t `(,@code1 ,@code2 ,(make-vm-op dest place1 (set-type-to-op operand-type ret-type base-op) place2))))))

(defun get-remote-dest (clause)
   (lookup-used-var (clause-get-remote-dest clause)))
   
(defun get-remote-reg-and-code (clause default)
   (if (clause-is-remote-p clause)
      (let ((var (get-remote-dest clause)))
         (if (reg-p var)
            var
            (with-reg (new-reg)
               (values new-reg `(,(make-move var new-reg)))))) 
      default))

(defun add-subgoal (subgoal reg &optional (in-c reg))
   (with-subgoal subgoal (:args args)
      (filter #L(not (null !1))
         (loop for arg in args
               for i upto (length args)
               collect (let ((already-defined (lookup-used-var (var-name arg))))
                        (cond
                           (already-defined (make-low-constraint (expr-type arg) (make-reg-dot in-c i) already-defined))
                           (t (add-used-var (var-name arg) (make-reg-dot reg i)) nil)))))))
                        
(defun compile-head-move (arg i tuple-reg)
   (let ((reg-dot (make-reg-dot tuple-reg i)))
      (compile-expr-to arg reg-dot)))
      
(defun do-compile-head-subgoals (head clause)
   (do-subgoals head (:name name :args args :operation append)
      (with-reg (tuple-reg)
         (let ((res `(,(make-vm-alloc name tuple-reg)
            ,@(loop for arg in args
                  for i upto (length args)
                  append (compile-head-move arg i tuple-reg))
            ,@(multiple-value-bind (send-to extra-code) (get-remote-reg-and-code clause tuple-reg)
               `(,@extra-code ,(make-send tuple-reg send-to))))))
            res))))

(defun compile-deletes (deletes)
   (loop for delete in deletes
         for id = (first delete)
         for expr = (second delete)
         append (with-compiled-expr (place instrs) expr
                  `(,@instrs ,(make-vm-delete id place)))))

(defun do-compile-head (head clause)
   ;; if clause has tuples to delete before generating the head
   ;; we do this here
   (let ((subgoal-code (do-compile-head-subgoals head clause)))
      (if (clause-has-delete-p clause)
         (let ((delete-code (compile-deletes (clause-get-delete clause))))
            `(,@subgoal-code ,@delete-code))
         subgoal-code)))
      
(defun compile-assignments-and-head (assignments head-fun)
   (if (null assignments)
       (funcall head-fun)
       (let ((ass (find-if (valid-assignment-p (all-used-var-names)) assignments)))
         (with-compiled-expr (place instrs) (assignment-expr ass)
            (add-used-var (var-name (assignment-var ass)) place)
            (let ((other-code (compile-assignments-and-head (remove-tree ass assignments) head-fun)))
               `(,@instrs ,@other-code))))))

(defun remove-defined-assignments (assignments) (mapcar #L(remove-used-var (var-name (assignment-var !1))) assignments))

(defun compile-head (body head clause)
   (let ((assigns (filter #'assignment-p body)))
      (always-ret (compile-assignments-and-head assigns #L(do-compile-head head clause))
         (remove-defined-assignments assigns))))
                     
(defun compile-iterate (body orig-body head clause)
   (multiple-value-bind (constraints assignments) (get-compile-constraints-and-assignments body)
      (let* ((next-sub (find-if #'subgoal-p body))
             (rem-body (remove-unneeded-assignments (remove-tree next-sub (remove-all body constraints)) head)))
         (compile-constraints-and-assignments constraints assignments
            (if (not next-sub)
               (compile-head rem-body head clause)
               (let ((next-sub-name (subgoal-name next-sub)))
                  (with-reg (reg next-sub)
                     (let* ((match-constraints (mapcar #'rest (add-subgoal next-sub reg :match)))
                            (iterate-code (compile-iterate rem-body orig-body head clause))
                            (other-code `(,(make-move :tuple reg) ,@iterate-code)))
                        `(,(make-iterate next-sub-name match-constraints other-code))))))))))
      
(defun compile-constraint (inner-code constraint)
   (let ((c-expr (constraint-expr constraint)))
      (with-compiled-expr (reg expr-code) c-expr
         `(,@expr-code ,(make-if reg inner-code)))))
               
(defun select-best-constraint (constraints all-vars)
   (let ((all (filter (valid-constraint-p all-vars) constraints)))
      (if (null all)
         nil
         (first (sort all #'> :key #'constraint-priority)))))
   
(defun do-compile-constraints-and-assignments (constraints assignments inner-code)
   (if (null constraints)
      inner-code
      (let* ((all-vars (all-used-var-names))
            (new-constraint (select-best-constraint constraints all-vars)))
         (if (null new-constraint)
            (let ((ass (find-if (valid-assignment-p all-vars) assignments)))
               (with-compiled-expr (place instrs) (assignment-expr ass)
               (add-used-var (var-name (assignment-var ass)) place)
               (let ((other-code (do-compile-constraints-and-assignments
                                    constraints (remove-tree ass assignments) inner-code)))
                  `(,@instrs ,@other-code))))
            (let ((inner-code (do-compile-constraints-and-assignments
                                       (remove-tree new-constraint constraints) assignments inner-code)))
               (compile-constraint inner-code new-constraint))))))

(defun compile-constraints-and-assignments (constraints assignments inner-code)
   (always-ret (do-compile-constraints-and-assignments constraints assignments inner-code)
      (remove-defined-assignments assignments)))

(defun compile-low-constraints (constraints inner-code)
   (with-reg (reg)
      (reduce #'(lambda (c old)
                  (list (make-vm-op reg (low-constraint-v1 c)
                                    (set-type-to-op (low-constraint-type c) :type-bool :equal)
                                    (low-constraint-v2 c))
                           (make-if reg old)))
               constraints :initial-value inner-code :from-end t)))

(defun compile-initial-subgoal (body orig-body head clause subgoal)
   (let ((without-subgoal (remove-tree subgoal body)))
      (if (null (subgoal-args subgoal))
         (compile-iterate without-subgoal orig-body head clause)
         (with-reg (sub-reg subgoal)
            (let ((start-code (make-move :tuple sub-reg))
                  (low-constraints (add-subgoal subgoal sub-reg))
                  (inner-code (compile-iterate without-subgoal orig-body head clause)))
               `(,start-code ,@(compile-low-constraints low-constraints inner-code)))))))
               
(defun get-my-subgoals (body name)
   (filter #L(equal (subgoal-name !1) name) (get-subgoals body)))
   
(defun compile-with-starting-subgoal (body head clause &optional subgoal)
   (let-compile
      (multiple-value-bind (first-constraints first-assignments) (get-compile-constraints-and-assignments body)
         (let* ((remaining (remove-unneeded-assignments (remove-all body first-constraints) head))
               (inner-code (compile-initial-subgoal remaining body head clause subgoal)))
            (compile-constraints-and-assignments first-constraints first-assignments inner-code)))))
            
(defun compile-normal-process (name clauses)
   (unless clauses (return-from compile-normal-process nil))
   (do-clauses clauses (:body body :head head :clause clause :operation append)
      (loop-list (subgoal (get-my-subgoals body name) :operation append)
         (compile-with-starting-subgoal body head clause subgoal))))
                  
(defun compile-init-process ()
   (unless (axioms) (return-from compile-init-process nil))
   (do-clauses (axioms) (:body body :head head :clause clause :operation :append)
      (compile-with-starting-subgoal body head clause)))
      
(defun compile-ast ()
   (do-definitions *ast* (:definition def :name name :operation collect)
      (if (is-init-p def)
         (make-process name `(,@(compile-init-process) ,(make-return)))
         (make-process name `(,@(compile-normal-process name (find-clause-with-body-subgoal name))
                              ,(make-return))))))