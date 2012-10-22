(in-package :cl-meld)

(defparameter *number-types* '(:type-int :type-float))
(defparameter *list-number-types* '(:type-list-int :type-list-float))
(defparameter *list-types* `(,@*list-number-types* :type-list-addr :type-list-worker))
(defparameter *all-types* `(,@*number-types* :type-bool :type-addr :type-worker ,@*list-types*))

(defmacro deftype-p (&rest types)
   `(on-top-level
         ,@(mapcar #'(lambda (x) `(defun ,(format-symbol t "TYPE-~A-P" (symbol-name x)) (ty)
                                       (eq ,(format-symbol "KEYWORD" "TYPE-~A" (symbol-name x)) ty)))
                  types)))

(deftype-p int addr worker bool string float list-int list-float list-addr list-worker)

(defun type-operands (op &optional forced-types)
   (cond
      ((eq-arith-p op)
         (if forced-types
            (intersection forced-types *number-types*)
            *number-types*))
      ((eq-cmp-p op)
         (if (or forced-types
                 (not (has-elem-p forced-types :type-bool)))
            `(,@*number-types* :type-addr :type-bool ,@*list-types*)))))

(defun type-op (op &optional forced-types)
   (cond
      ((eq-arith-p op)
         (if forced-types
            (intersection *number-types* forced-types)
            '*number-types*))
      ((eq-cmp-p op)
         (if forced-types
            (intersection '(:type-bool) forced-types)
            '(:type-bool)))))
            
(defun type-oper-op (op forced-types)
   (cond
      ((eq-arith-p op)
         (intersection *number-types* forced-types))
      ((eq-cmp-p op) '(:type-bool))))
      
(defun expr-type (expr)
   (cond
      ((or (nil-p expr) (host-id-p expr)) (second expr))
      ((or (var-p expr) (int-p expr) (float-p expr) (addr-p expr) (tail-p expr)
           (head-p expr) (not-p expr) (test-nil-p expr)
           (convert-float-p expr)
			  (get-constant-p expr))
         (third expr))
      ((or (op-p expr) (call-p expr) (cons-p expr))
         (fourth expr))
      ((or (let-p expr) (if-p expr)) (fifth expr))
      (t (error 'type-invalid-error :text (tostring "Cannot deduce type of expression ~a" expr)))))
      
(defun typed-var-p (var) (and (= (length var) 3)))
(defun single-typed-var-p (var) (and (typed-var-p var) (one-elem-p (third var))))
(defun typed-op-p (op) (= (length op) 4))
(defun typed-int-p (i) (= (length i) 3))

(defun same-types-p (types1 types2)
	(set-equal-p types1 types2))
