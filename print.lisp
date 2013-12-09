(in-package :cl-meld)

(defun print-val-list (vals)
	(let ((args (mapcar #'print-val vals))
			(start ""))
		(dolist-count (arg args id)
      	(when (> id 1)
       		(setf start (concatenate 'string start ", ")))
      	(setf start (concatenate 'string start arg)))
		start))

(defun print-val (val &optional (hide-pars nil))
	(cond
		((bool-p val) (tostring "~a" (if (bool-val val) "true" "false"))) 
      ((var-p val) (tostring "~a" (var-name val)))
      ((int-p val) (tostring "~A" (int-val val)))
      ((float-p val) (tostring "~A" (float-val val)))
      ((host-id-p val) (tostring "host-id"))
      ((world-p val) (tostring "world"))
      ((addr-p val) (tostring "@~a" (addr-num val)))
      ((string-constant-p val) (tostring "\"~a\"" (string-constant-val val)))
		((struct-p val)
			(tostring ":(~a)" (print-val-list (struct-list val))))
 		((struct-val-p val) (tostring "~a:~a" (print-val (struct-val-var val)) (struct-val-idx val)))
		((convert-float-p val)
         (tostring "float(~a)" (print-val (convert-float-expr val))))
      ((cons-p val)
         (tostring "cons(~a,~a)" (print-val (cons-head val)) (print-val (cons-tail val))))
      ((tail-p val)
         (tostring "tail(~a)" (print-val (tail-list val))))
      ((head-p val)
         (tostring "head(~a)" (print-val (head-list val))))
      ((nil-p val) (tostring "nil"))
      ((test-nil-p val)
         (tostring "test-nil(~a)" (print-val (test-nil-expr val))))
      ((not-p val)
         (tostring "not(~a)" (print-val (not-expr val))))
      ((op-p val)
         (tostring "~a~a ~a ~a~a"
            (if hide-pars "" "(")
            (print-val (op-op1 val))
            (op-to-string (op-op val))
            (print-val (op-op2 val))
            (if hide-pars "" ")")))
      ((let-p val)
         (tostring "let ~a = ~a in ~a end" (print-val (let-var val)) (print-val (let-expr val)) (print-val (let-body val))))
      ((if-p val)
         (tostring "if ~a then ~a else ~a end" (print-val (if-cmp val)) (print-val (if-e1 val)) (print-val (if-e2 val))))
      ((get-constant-p val)
         (with-constant (lookup-const (get-constant-name val)) (:expr expr)
            (print-val expr)))
      ((or (call-p val) (callf-p val))
			(tostring "~a(~a)" (call-name val) (print-val-list (call-args val))))
      ((argument-p val)
         (tostring "@arg~a" (argument-id val)))
      (t
         (error 'expr-invalid-error :text (tostring "Can't print expression ~a" val)))))
   
(defun print-args (stream args)
   (format stream "(")
   (dolist-count (arg args id)
      (if (> id 1)
         (format stream ", "))
      (format stream "~a" (print-val arg t)))
   (format stream ")"))

(defparameter *current-print-level* 0)
(defparameter *max-print-level* 4)
(defvar *number-of-items-printted* 0)

(defmacro check-print-level (stream)
 `(progn
      (when (> *number-of-items-printted* 0)
         (format ,stream ", "))
      (incf *number-of-items-printted*)
      (cond
         ((= *current-print-level* *max-print-level*)
            (setf *current-print-level* 0)
            (format ,stream "~a~a~a~a" #\Newline #\Tab #\Tab #\Tab))
         (t (incf *current-print-level*)))))

(defun print-subgoal-modifier (sub def)
	(cond
		((is-linear-p def)
			(if (subgoal-has-option-p sub :reuse)
				"!"
				(if (subgoal-has-option-p sub :linear)
					"?"
					"")))
		(t
			"!")))
      
(defun print-subgoals (stream subgoals)
   (do-subgoals subgoals (:name name :args args :subgoal sub)
      (with-definition (lookup-definition name) (:definition def)
         (check-print-level stream)
         (format stream "~a~A" (print-subgoal-modifier sub def) name)
         (print-args stream args)
			(when (subgoal-is-remote-p sub)
				(format stream "@~a" (subgoal-get-remote-dest sub)))
			)))
      
(defun print-constraints (stream subgoals)
   (do-constraints subgoals (:expr expr :id id)
      (check-print-level stream)
      (format stream "~a" (print-val expr t))))

(defun print-assignments (stream subgoals)
   (do-assignments subgoals (:var var :expr expr :id id)
      (check-print-level stream)
      (format stream "~a" (print-val var t))
      (format stream " = ")
      (format stream "~a" (print-val expr t))))
      
(defun print-type (typ)
   (cond
      ((type-int-p typ) "int")
      ((type-float-p typ) "float")
      ((type-addr-p typ) "addr")
      ((type-string-p typ) "string")
		((type-list-p typ)
			(let ((sub (type-list-element typ)))
				(tostring "list ~a" (print-type sub))))
      (t (error 'expr-invalid-error :text (tostring "print-type does not know how to handle ~a" typ)))))
      
(defun print-types (typs)
   (reduce #'(lambda (all new)
               (let ((typ (print-type new)))
                  (if (string-equal all "")
                     typ
                     (tostring "~a, ~a" all typ))))
                  typs :initial-value ""))

(defmacro with-comma-context (&body body)
   `(let ((*number-of-items-printted* 0))
      ,@body))

(defmacro with-print-context (&body body)
   `(let ((*current-print-level* 0) (*number-of-items-printted* 0))
      ,@body))

(defun print-subgoal-body (stream body)
   (with-comma-context
      (print-subgoals stream body)
      (when (has-assignments-p body)
         (print-assignments stream body))
      (when (has-constraints-p body)
         (print-constraints stream body))))

(defun print-var-list (stream vars)
   (dolist-count (var vars i)
     (when (> i 1)
         (format stream ", "))
     (format stream "~a" (print-val var))))
   
(defun print-clause (stream clause)
	(with-clause clause (:head head :body body)
      (with-print-context
         (print-subgoal-body stream body)
         (format stream " -o ")
         (with-comma-context
				(when (null head)
					(format stream "1"))
            (print-subgoals stream head)
            (do-comprehensions head (:right right :left left :variables vars)
               (check-print-level stream)
               (with-comma-context
                  (format stream "{")
                  (print-var-list stream vars)
                  (format stream " | ")
                  (print-subgoal-body stream left)
                  (format stream " | ")
                  (check-print-level stream)
                  (with-comma-context
                     (print-subgoals stream right))
                  (format stream "}")))
            (do-agg-constructs head (:body body :head head :vlist vars :specs specs)
               (check-print-level stream)
               (with-comma-context
						(format stream "[ ")
						(do-agg-specs specs (:var to :op op)
                  	(format stream "~A => ~a, " op (print-val to)))
                  (format stream " | ")
						(print-var-list stream vars)
                  (format stream " | ")
                  (check-print-level stream)
                  (with-comma-context
                     (print-subgoal-body stream body))
                  (format stream " | ")
                  (with-comma-context
                   (print-subgoals stream head))
                  (format stream "]")))
				(do-exists head (:var-list vars :body body)
					(check-print-level stream)
					(format stream "exists ")
					(print-var-list stream vars)
					(format stream ". (")
					(with-comma-context
						(print-subgoals stream body))
					(format stream ")"))
            (format stream ".")))))

(defun clause-to-string (clause)
   (with-output-to-string (str)
      (print-clause str clause)))
         
(defun print-program ()
   (format t "I found the following definitions:~%")
   (do-definitions (:name name :types typs)
      (format t "~A [~A]~%" name (print-types typs)))
   (format t "I found the following axioms:~%")
   (do-axioms (:clause clause :id id)
      (print-clause t clause)
      (format t "~%"))
   (format t "I found the following rules:~%")
   (do-rules (:clause clause :id id)
      (format t "Clause ~A: " id)
      (print-clause t clause)
      (format t "~%")))
