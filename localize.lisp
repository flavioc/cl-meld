(in-package :cl-meld)

(define-condition localize-invalid-error (error)
   ((text :initarg :text :reader text)))
   
(defvar *route-facts-to-invert* nil)
(defun add-route-fact-to-invert (fact)
   (push-dunion fact *route-facts-to-invert*))

(defun get-first-arg (subgoal)
   (first (subgoal-args subgoal)))
(defun get-second-arg (subgoal)
   (second (subgoal-args subgoal)))

(defun get-paths (subgoals routes)
   (filter #L(has-test-elem-p routes (subgoal-name !1) #'string-equal) (get-subgoals subgoals)))

(defun equal-to-any-home (arg homes)
   (some #L(var-eq-p arg !1) homes))
(defun select-subgoals-by-home (subgoals home-vars)
   (filter #L(equal-to-any-home (get-first-arg !1) home-vars) (get-subgoals subgoals)))

(defun generate-args (n typs)
   (mapcar #L(make-var (concatenate 'string "X" (write-to-string !2)) !1) typs (enumerate 0 (1- n))))
(defun generate-inverse-name (name)
   (concatenate 'string "___" (reverse name)))
(defun swap-first-two-args (args)
   `(,(second args) ,(first args) ,@(rest (rest args))))   
(defun change-first-arg (args first)
   `(,first ,@(rest args)))

(defparameter *name-counter* 0)
(defun generate-mangled-name ()
   (with-output-to-string (a) (format a "__mangledname~a" (incf *name-counter*))))

(defun create-inverse-non-fact-route-definition (route new-name)
   (let* ((old-definition (lookup-definition-types route))
          (new-definition (make-definition new-name old-definition (list :route `(:reverse-route ,route)))))
      (push-end new-definition *definitions*)
      new-definition))
      
(defun create-inverse-non-fact-route (route new-name new-definition)
   (let* ((typs (definition-arg-types (definition-types new-definition)))
          (args (generate-args (length typs) typs))
			 (sub 	(make-subgoal new-name
                           (swap-first-two-args args)))
          (new-clause (make-clause `(,(make-subgoal route args))
                  `(,sub)
                  `(:route ,(var-name (second args))))))
		(subgoal-add-route sub (var-name (second args)))
      (push new-clause *clauses*)))

(defun add-inverse-route-facts (route new-name)
   (with-ret to-ret
      (do-axioms (:head head :body body)
         (do-subgoals head (:name name :args args)
            (if (equal route name)
               (let* ((reverse-host (second args))
                      (host-var (first args))
                      (constraint-expr (first (find-assignment-constraints body host-var)))
                      (real-host (op-op2 constraint-expr))
                      (remain-args (drop-first-n args 2))
                      (new-clause (make-clause nil `(,(make-subgoal new-name `(,reverse-host ,real-host ,@remain-args)))))) 
                  (add-variable-head-clause new-clause)
                  (push new-clause to-ret)))))))
      
(defun create-inverse-routes ()
   (dolist (route *route-facts-to-invert*)
      (let* ((new-name (generate-inverse-name route))
             (new-definition (create-inverse-non-fact-route-definition route new-name)))
         (if (is-fact-p route)
				(let ((new-ones (add-inverse-route-facts route new-name)))
					(if new-ones
                 	(setf *axioms* (append new-ones *axioms*))
						(create-inverse-non-fact-route route new-name new-definition)))
            (create-inverse-non-fact-route route new-name new-definition)))))
         
(defun select-valid-constraints (body vars)
   (filter #L(subsetp (all-variable-names !1) vars) (get-constraints body)))
(defun generate-inverse-subgoal (new-name to needed-vars)
   (make-subgoal new-name `(,to ,@needed-vars) `((:route ,(var-name to)))))

(defun match-paths (sources part) #'(lambda (path) (some #L(var-eq-p !1 (funcall part path)) sources)))
(defun expand-sources (sources paths)
   (append (mapfilter #'second (match-paths sources #'first) paths)
           (mapfilter #'first (match-paths sources #'second) paths)))
(defun decrease-paths (sources paths)
   (remove-if (match-paths sources #'second)
              (remove-if (match-paths sources #'first) paths)))
(defun host-node (head) (get-first-arg (first head)))

(defun find-all-addrs-in-subgoal (subgoal)
	(with-subgoal subgoal (:name name :args args)
		(let ((types (lookup-definition-types name)))
			(loop for typ in (rest types)
					for arg in (rest args)
					when (type-addr-p typ)
					collect (list (first args) arg)))))

(defun get-reachable-nodes (paths-sub start-node)
   (let ((paths (mappend #L(find-all-addrs-in-subgoal !1) paths-sub))
          (rm `(,start-node)))
      (loop while paths
            for expand = (expand-sources rm paths)
            do (unless expand
                  (error 'localize-invalid-error :text (tostring "Invalid paths in clause: ~a" paths-sub)))
               (setf paths (decrease-paths rm paths)
                     rm (append rm expand)))
      rm))
               
(defun variables-defined-on-body (body host &optional except)
   (with-ret ret
      (push-dunion host ret)
      (do-assignments body (:var var) (push-dunion var ret))
      (do-subgoals body (:args args)
         (dolist (arg args)
            (when (and (var-p arg)
                       (or (null except)
                           (not (var-eq-p except arg))))
               (push-dunion arg ret))))))
               
(defun variables-undefined-on-head-and-body (head body)
   (with-ret ret
      (push-dunion-all (all-variables head) ret)
      (do-constraints body (:expr expr)
         (push-dunion-all (all-variables expr) ret))
      (do-assignments body (:expr expr)
         (push-dunion-all (all-variables expr) ret))
      (do-subgoals body (:args args)
         (dolist (arg args)
            (push-dunion-all (all-variables arg) ret)))))
                     
(defun variables-undefined-head (head body host)
   (set-tree-difference (variables-undefined-on-head-and-body head body) (list host)))

(defun get-inverse-route (route-subgoal)
   (make-subgoal (generate-inverse-name (subgoal-name route-subgoal))
                (swap-first-two-args (subgoal-args route-subgoal))))
                
(defun order-variables (vars)
   "Sorts variables by putting integer variables before every other."
   (sort vars #'(lambda (var1 var2)
                  (declare (ignore var2))
                  (type-int-p (expr-type var1)))))
                  
(defun any-linear-fact-p (body)
   (some #L(with-subgoal !1 (:name name)
            (let ((def (lookup-definition name)))
               (is-linear-p def)))
         (get-subgoals body)))
         
(defun transform-remote-subgoals (head host)
	(let ((all-transformed t))
      (do-subgoals head (:args args :subgoal sub)
         (let ((first-arg (first args)))
            (if (var-eq-p first-arg host)
               (setf all-transformed nil)
					(subgoal-add-route sub (var-name first-arg)))))
      (do-comprehensions head (:right right :comprehension comp)
			(do-subgoals right (:args args :subgoal sub)
            (let ((first-arg (first args)))
               (if (var-eq-p first-arg host)
                  (setf all-transformed nil)
						(subgoal-add-route sub (var-name first-arg))))))
		(do-exists head (:var-list vars :body body)
			(unless (transform-remote-subgoals body host)
				(setf all-transformed nil)))
      all-transformed))

(defun do-localize-one (clause from to route-subgoal remaining &optional (order 'forward))
   (let* ((reachable (get-reachable-nodes remaining to))
          (subgoals (select-subgoals-by-home (clause-body clause) reachable)))
      (unless subgoals
			;; all subgoals in host node
         (when (transform-remote-subgoals (clause-head clause) from)
				; every subgoal in head goes to 'from'
				(clause-add-option clause `(:route ,(var-name to))))
         (return-from do-localize-one nil))
      (let* ((body (clause-body clause))
             (head (clause-head clause))
             (assignments (select-valid-assignments body subgoals))
             (constraints (select-valid-constraints body (all-variable-names `(,@subgoals ,@assignments))))
             (stripped-body (remove-all body `(,route-subgoal ,@subgoals ,@constraints ,@assignments)))
             (new-routing (if (eq order 'forward) (get-inverse-route route-subgoal) nil)))
			(cond
            ((and (eq order 'backward)
                  (null stripped-body))
               (add-route-fact-to-invert (subgoal-name route-subgoal))
               (if (transform-remote-subgoals (clause-head clause) from)
                  (clause-add-option clause `(:route ,(var-name from))))
               (values clause nil))
            ((and (eq order 'forward)
                  (null stripped-body))
               (add-route-fact-to-invert (subgoal-name route-subgoal))
               (setf stripped-body `(,new-routing ,@assignments ,@constraints ,@subgoals))
               (when (transform-remote-subgoals (clause-head clause) to)
                  (clause-add-option clause `(:route ,(var-name from))))
               (setf (clause-body clause) stripped-body)
               (values clause nil))
            (t
					(let ((new-clause-body `(,@subgoals ,@assignments ,@constraints)))
						(when (is-route-subgoal-alone-p new-clause-body route-subgoal from)
							(reverse-route-subgoal-alone route-subgoal)
							(transform-remote-subgoals (clause-head clause) from)
							(return-from do-localize-one nil))
               	(when new-routing
                  	(add-route-fact-to-invert (subgoal-name route-subgoal))
                  	(push new-routing new-clause-body)
							(push new-routing subgoals))
               	(let* ((variables-undef-head (variables-undefined-head head stripped-body from))
                      	(variables-subgoals (variables-defined-on-body new-clause-body to))
                      	(needed-vars (order-variables (tree-intersection variables-subgoals variables-undef-head)))
                      	(new-subgoal (generate-inverse-subgoal (generate-mangled-name)
                                                from needed-vars)))
							(setf (clause-body clause) (remove-unneeded-assignments `(,new-subgoal ,@stripped-body) head))
							(let* ((new-clause-head `(,(copy-tree new-subgoal)))
                         	(new-clause-body (remove-unneeded-assignments new-clause-body new-clause-head))
                         	(route-to (var-name from)))
                     	(with-subgoal new-subgoal (:name name)
                        	(let* ((linear-facts-exist (any-linear-fact-p new-clause-body))
                               	(linear-prop (if linear-facts-exist '(:linear) nil)))
                           	(push-end (make-definition name `(:type-addr ,@(mapcar #'expr-type needed-vars))
                                    	`(:routed-tuple ,@linear-prop))
                                 	*definitions*)))
                     	(values (make-clause new-clause-body new-clause-head `(:route ,route-to))
                              	t)))))))))
            
(defun get-direction-and-dest (host edge)
   (if (var-eq-p host (get-first-arg edge))
       (values 'forward (get-second-arg edge))
       (values 'backward (get-first-arg edge))))
          
(defun do-localize (host clause edges remaining)
   "From node HOST in clause CLAUSE localize from EDGES"
   (dolist (edge edges)
      (multiple-value-bind (order to) (get-direction-and-dest host edge)
         (let* ((fun (edges-equal-to to))
                (new-edges (filter fun remaining))
                (new-remaining (remove-if fun remaining)))
            (multiple-value-bind (target-clause add-to-program-p)
                                    (do-localize-one clause host to edge remaining order)
               (when target-clause
                  (if add-to-program-p
                     (push target-clause *clauses*))
						(clause-set-persistent target-clause)
                  (when new-edges
                     (do-localize to target-clause new-edges new-remaining))))))))

(defun check-subgoal-arguments (homes clause)
	"Check that the clause only uses certain nodes in 'homes'"
   (do-subgoals clause (:args args :name name)
      (unless (some #'(lambda (h) (var-eq-p (first args) h)) homes)
         (error 'localize-invalid-error
               :text (tostring "Subgoal ~a has a bad home argument: ~a" name (first args))))))

(defun body-shares-same-home (body home-argument)
	(do-subgoals body (:args args :name name)
		(unless (var-eq-p home-argument (first args))
			(return-from body-shares-same-home nil)))
	t)
   
(defun edges-equal-to (host)
   #L(or (var-eq-p host (get-first-arg !1)) (var-eq-p host (get-second-arg !1))))

(defun find-linear-body-homes (clause homes)
	(let ((vars1 (iterate-expr #'(lambda (x)
                                 (cond
                                    ((and (var-p x) (type-addr-p (var-type x))) x))) clause)))
		(remove-duplicates (append vars1 homes) :test #'var-eq-p)))

(defun localize-start (clause routes host)
   (let ((paths (get-paths (clause-body clause) routes)))
      (let ((home-arguments (get-reachable-nodes paths host))
				(same-home nil))
			(when (and (one-elem-p home-arguments)
						(body-shares-same-home (clause-body clause) (first home-arguments)))
				;; When using the same home argument in the body of the rule
				;; we may use all the node variables in the body
				(setf same-home t)
				(setf home-arguments (find-linear-body-homes clause home-arguments)))
         (localize-check-head (clause-head clause) clause home-arguments host)
         (check-subgoal-arguments home-arguments clause)
			(when same-home
				(transform-remote-subgoals (clause-head clause) host))
			(unless same-home
         	(let* ((fun (edges-equal-to host))
                	 (edges (filter fun paths))
                	 (remaining (remove-if fun paths)))
					(if edges
               	(do-localize host clause edges remaining)
               	(transform-remote-subgoals (clause-head clause) host)))))))
      
(defun one-of-the-vars-p (ls var)
   (find-if #L(var-eq-p var !1) ls))

(defun is-route-subgoal-alone-p (body sub host)
	(with-subgoal sub (:args args)
		(let ((def (lookup-subgoal-definition sub)))
			(and (is-route-p def)
					(= (length (get-subgoals body)) 1)
					(var-eq-p host (second args))))))
					
(defun reverse-route-subgoal-alone (sub)
	(with-subgoal sub (:name name :args args)
		(let ((inverse-name (generate-inverse-name name)))
			(setf (subgoal-name sub) inverse-name)
			(setf (subgoal-args sub) (swap-first-two-args args)))
		(add-route-fact-to-invert name)))

(defun check-remote-args-in-constructs (body host)
	(do-subgoals body (:args args :subgoal sub :name name)
      (let ((first-arg (first args)))
         (unless (and (var-p first-arg)
                        (var-eq-p host first-arg))
				(cond
					((is-route-subgoal-alone-p body sub host)
						(reverse-route-subgoal-alone sub))
               (t (error 'localize-invalid-error
                        	:text (tostring "Variable is not host: ~a" first-arg))))))))

(defun localize-check-head-by-homes (head homes)
	(do-subgoals head (:args args)
      (let ((first-arg (first args)))
         (unless (and (var-p first-arg)
                     (one-of-the-vars-p homes first-arg))
            (error 'localize-invalid-error
                     :text (tostring "Variable was not found: ~a (available: ~{~a~^, ~})" (var-name first-arg) (mapcar #'var-name homes)))))))

(defun localize-check-head (head clause homes host)
	(localize-check-head-by-homes head homes)
	(do-agg-constructs head (:body body :agg-construct c)
		(check-remote-args-in-constructs body host))
   (do-comprehensions head (:left left)
		(check-remote-args-in-constructs left host))
	(do-exists head (:var-list vars :body body)
		(localize-check-head-by-homes body (append vars homes))))

(defun remove-home-argument-clause (clause)
   (let ((host (clause-host-node clause)))
      (transform-drop-subgoal-first-arg clause host)
      (transform-variable-to-host-id clause host)))
         
(defun remove-home-argument ()
   (do-rules (:clause clause)
      (remove-home-argument-clause clause))
   (do-axioms (:clause clause)
      (remove-home-argument-clause clause))
   (do-node-definitions (:definition def :types typs)
      (setf (definition-types def) (rest typs))))

(defmacro with-localize-context ((routes) &body body)
   `(let ((,routes (get-route-names))
          (*route-facts-to-invert* nil))
      ,@body))

(defun localize ()
   (with-localize-context (routes)
      (do-rules (:clause clause :head head)
         (localize-start clause routes (clause-body-host-node clause)))
      (create-inverse-routes))
   (remove-home-argument))
