
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
   "Traverses the entire expression.
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