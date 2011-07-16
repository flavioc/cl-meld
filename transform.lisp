
(in-package :cl-meld)

(defmacro transform-part-expression (part &optional (stop-here nil))
   (with-gensyms (x)
      `(with-symbol (,x ,part)
         (if (funcall test-fn ,x)
            (multiple-value-bind (new-val stop-p) (funcall transform-fn ,x)
               ,@(when stop-here
                     `((declare (ignore stop-p))))
               (when new-val (setf ,x new-val))
               ,@(unless stop-here
                  `((when (eq stop-p :stop)
                        (transform-expr test-fn transform-fn ,x))))))
            ,@(unless stop-here
               `((transform-expr test-fn transform-fn ,x))))))

(defun transform-expr (test-fn transform-fn expr)
   "Traverses the entire expression and changes it.
   For each sub-expression, test-fn is called.
   If the test returns T, we execute transform-fn for the sub-expression.
   The return value must be two-valued:
      1st - The new value for this sub-expression (if nil no change is done)
      2nd - The :stop symbol, which stops the iteration for deeper sub-expressions inside this sub-expression"
   (unless expr
      (return-from transform-expr nil))
   (cond
      ;; we do nothing for these
      ((var-p expr)) ((int-p expr))
      ((float-p expr)) ((host-id-p expr))
      ((nil-p expr)) ((world-p expr))
      ((addr-p expr))
      ((clause-p expr)
         (transform-expr test-fn transform-fn (clause-head expr))
         (transform-expr test-fn transform-fn (clause-body expr)))
      ((subgoal-p expr)
         (loop-cons-car (arg (subgoal-args expr))
            (transform-part-expression arg)))
      ((constraint-p expr) (transform-part-expression (constraint-expr expr)))
      ((assignment-p expr)
         (transform-part-expression (assignment-var expr) t)
         (transform-part-expression (assignment-expr expr)))
      ((call-p expr)
         (loop-cons-car (arg (call-args expr))
            (transform-part-expression arg)))
      ((cons-p expr)
         (transform-part-expression (cons-head expr))
         (transform-part-expression (cons-tail expr)))
      ((head-p expr) (transform-part-expression (head-list expr)))
      ((tail-p expr) (transform-part-expression (tail-list expr)))
      ((not-p expr) (transform-part-expression (not-expr expr)))
      ((test-nil-p expr) (transform-part-expression (test-nil-expr expr)))
      ((convert-float-p expr) (transform-part-expression (convert-float-expr expr)))
      ((colocated-p expr)
         (transform-part-expression (colocated-first expr))
         (transform-part-expression (colocated-second expr)))
      ((op-p expr)
         (transform-part-expression (op-op1 expr))
         (transform-part-expression (op-op2 expr)))
      ((list-of-lists-p expr)
         (loop-cons-car (e expr)
            (transform-part-expression e)))
      (t (error 'expr-invalid-error
               :text (tostring "Invalid expression: ~a" expr))))
   expr)
   
(defmacro do-map-expr (expr)
   `(map-expr test-fn map-fn ,expr :go-down-fn go-down-fn))

(defmacro map-atom-expr ()
   `(if (funcall test-fn expr)
      (funcall map-fn expr)
      expr))
      
(defmacro with-mapped-expr (&body body)
   `(cond
      ((funcall test-fn expr)
       (funcall map-fn expr))
      ((funcall go-down-fn expr)
       ,@body)
      (t expr)))

(defun map-expr (test-fn map-fn expr &key (go-down-fn #'always-true))
   "Traverses the expression and creates a new expression.
   This is the functional counterpart of transform-expr.
   Parameters:
      - test-fn: called to check if map-fn is to be called upon the expression.
      - map-fn: called after test-fn succeeds. transforms the expression into something.
      - go-down-fn: called to check if we should go down into sub-expressions.
                    only called if test-fn returns nil.
      - expr: the expression to be mapped."
   (unless expr
      (return-from map-expr nil))
   (cond
      ((var-p expr) (map-atom-expr))
      ((int-p expr) (map-atom-expr))
      ((float-p expr) (map-atom-expr))
      ((host-id-p expr) (map-atom-expr))
      ((nil-p expr) (map-atom-expr))
      ((world-p expr) (map-atom-expr))
      ((addr-p expr) (map-atom-expr))
      ((clause-p expr)
         (with-mapped-expr
            (make-clause (do-map-expr (clause-body expr))
                         (do-map-expr (clause-head expr))
                         (clause-options expr))))
      ((subgoal-p expr)
         (with-mapped-expr
            (make-subgoal (subgoal-name expr)
                          (mapcar #L(do-map-expr !1) (subgoal-args expr)))))
      ((constraint-p expr)
         (with-mapped-expr
            (make-constraint (do-map-expr (constraint-expr expr))
                             (constraint-priority expr))))
      ((assignment-p expr)
         (with-mapped-expr
            (make-assignment (do-map-expr (assignment-var expr))
                             (do-map-expr (assignment-expr expr)))))
      ((call-p expr)
         (with-mapped-expr
            (make-call (call-name expr)
                       (mapcar #L(do-map-expr !1) (call-args expr)))))
      ((cons-p expr)
         (with-mapped-expr
            (make-cons (do-map-expr (cons-head expr))
                       (do-map-expr (cons-tail expr)))))
      ((head-p expr)
         (with-mapped-expr
            (make-head (do-map-expr (head-list expr)))))
      ((tail-p expr)
         (with-mapped-expr
            (make-tail (do-map-expr (tail-list expr)))))
      ((not-p expr)
         (with-mapped-expr
            (make-not (do-map-expr (not-expr expr)))))
      ((test-nil-p expr)
         (with-mapped-expr
            (make-test-nil (do-map-expr (test-nil-expr expr)))))
      ((convert-float-p expr)
         (with-mapped-expr
            (make-convert-float (do-map-expr (convert-float-expr expr)))))
      ((colocated-p expr)
         (with-mapped-expr
            (make-colocated (do-map-expr (colocated-first expr))
                            (do-map-expr (colocated-second expr)))))
      ((op-p expr)
         (with-mapped-expr
            (make-op (op-op expr)
                     (do-map-expr (op-op1 expr))
                     (do-map-expr (op-op2 expr)))))
      ((list-of-lists-p expr)
       (loop for item in expr
             collect (do-map-expr item)))
      (t (assert nil))))

(defun transform-drop-subgoal-first-arg (expr)
   (transform-expr #'subgoal-p #'(lambda (x)
                                    (setf (subgoal-args x) (rest (subgoal-args x)))
                                    (values nil :stop))
                  expr))

(defun transform-variable-to-host-id (expr old-var)
   (let ((host-id (make-host-id)))
      (transform-expr #'(lambda (x) (var-eq-p x old-var))
                      #'(lambda (var)
                           (declare (ignore var))
                           (values host-id :stop))
                     expr)))