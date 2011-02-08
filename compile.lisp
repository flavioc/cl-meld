(in-package :cl-meld)

(defun get-matching-clauses (subgoal-name code)
   (filter #L(some #'(lambda (sub) (equal (subgoal-name sub) subgoal-name)) (clause-body !1)) (clauses code)))

(defun alloc-reg (regs data)
   (let ((reg (make-reg (length regs))))
      (values reg (cons data regs))))

(defun get-my-subgoal (body name) (filter #L(equal (subgoal-name !1) name) (get-subgoals body)))

(defparameter *vars-places* nil)
(defparameter *used-regs* nil)
(defparameter *clause-head* nil)

(defun alloc-new-reg (data) (alloc-reg *used-regs* data))

(defun init-vars-places () (make-hash-table))
(defun lookup-used-var (var-name)
   (multiple-value-bind (data found) (gethash var-name *vars-places*)
      (when found data)))
(defun add-used-var (var-name data) (setf (gethash var-name *vars-places*) data))
(defun all-used-var-names () (hash-table-keys *vars-places*))

(defun valid-constraint-p (all-vars) #L(subsetp (all-variable-names !1) all-vars))
(defun get-compile-constraints (body)
   (filter (valid-constraint-p (all-used-var-names)) (get-constraints body)))

(defun make-low-constraint (typ v1 v2) `(,typ ,v1 ,v2))
(defun low-constraint-type (lc) (first lc))
(defun low-constraint-v1 (lc) (second lc))
(defun low-constraint-v2 (lc) (third lc))

(defun add-subgoal (subgoal reg &optional (in-c reg))
   (with-subgoal subgoal (:args args)
      (filter #L(not (null !1))
         (loop for arg in args
               for i upto (length args)
               collect (let ((already-defined (lookup-used-var (var-name arg))))
                        (cond
                           (already-defined (make-low-constraint (expr-type arg) (make-reg-dot in-c i) already-defined))
                           (t (add-used-var (var-name arg) (make-reg-dot reg i)) nil)))))))

(defun compile-head (head)
   (with-ret ret
      (do-subgoals head (:name name :args args)
         (multiple-value-bind (tuple-reg *used-regs*) (alloc-new-reg nil)
            (setf ret `(,@ret ,(make-vm-alloc name tuple-reg)))
            (loop for arg in args
                  for i upto (length args)
                  do (multiple-value-bind (reg arg-code) (compile-expr arg)
                        (setf ret `(,@ret ,@arg-code ,(make-move reg (make-reg-dot tuple-reg i))))))))))
   
(defun compile-iterate (body)
   (let ((constraints (get-compile-constraints body))
         (next-sub (find-if #'subgoal-p body)))
      (compile-constraints constraints
         (if (not next-sub)
            (compile-head *clause-head*)
            (let ((next-sub-name (subgoal-name next-sub))
                  (remaining (remove-tree next-sub (remove-all body constraints))))
               (multiple-value-bind (reg *used-regs*) (alloc-reg *used-regs* next-sub)
                  (let ((match-constraints (mapcar #'rest (add-subgoal next-sub reg :match)))
                        (other-code `(,(make-move :tuple reg) ,@(compile-iterate remaining))))
                     `(,(make-iterate next-sub-name match-constraints other-code)))))))))

(defun compile-expr (expr)
   (cond
      ((int-p expr) (values (make-vm-int (int-val expr)) nil *used-regs*))
      ((var-p expr) (values (lookup-used-var (var-name expr)) nil *used-regs*))
      ((op-p expr)
         (multiple-value-bind (place1 code1 *used-regs*) (compile-expr (op-op1 expr))
            (multiple-value-bind (place2 code2 *used-regs*) (compile-expr (op-op2 expr))
               (multiple-value-bind (new-reg *used-regs*) (alloc-reg *used-regs* expr)
                  (let* ((op (set-type-to-op (expr-type (op-op1 expr)) (expr-type expr) (op-op expr)))
                         (set-instr (make-set new-reg place1 op place2)))
                     (values new-reg `(,@code1 ,@code2 ,set-instr) *used-regs*))))))))
            
(defun compile-constraint (inner-code constraint)
   (multiple-value-bind (reg expr-code) (compile-expr (constraint-expr constraint))
      `(,@expr-code ,(make-if reg inner-code))))
      
(defun compile-constraints (constraints inner-code)
   (reduce #L(compile-constraint !1 !2) constraints :initial-value inner-code))

(defun compile-low-constraints (constraints inner-code)
   (let ((reg (alloc-reg *used-regs* nil)))
      (reduce #'(lambda (c old)
                  (list (make-set reg (low-constraint-v1 c)
                                    (set-type-to-op (low-constraint-type c) :type-bool :equal)
                                    (low-constraint-v2 c))
                           (make-if reg old)))
               constraints :initial-value inner-code :from-end t)))

(defun compile-initial-subgoal (body subgoal)
   (multiple-value-bind (sub-reg *used-regs*) (alloc-reg *used-regs* subgoal)
      (let ((start-code (make-move :tuple sub-reg))
            (low-constraints (add-subgoal subgoal sub-reg))
            (inner-code (compile-iterate (remove-tree subgoal body))))
         `(,start-code ,@(compile-low-constraints low-constraints inner-code)))))

(defun build-process (name clauses)
   (unless clauses
      (return-from build-process nil))
   (letret (ret nil)
      (format t "tuple: ~a~%" name)
      (do-clauses clauses (:body body :head *clause-head*)
         (let* ((subgoal (first (get-my-subgoal body name)))
                (*vars-places* (init-vars-places))
                (*used-regs* nil)
                (first-constraints (get-compile-constraints body))
                (inner-code (compile-initial-subgoal (remove-all body first-constraints) subgoal))
                (new-code (compile-constraints first-constraints inner-code)))
            (setf ret (append ret new-code))))))

(defun compile-ast (code)
   (with-ret ret
      (do-definitions code (:name name)
         (let* ((clauses (get-matching-clauses name code))
                (new-proc (make-process name `(,@(build-process name clauses) ,(make-return)))))
         (push new-proc ret)))))