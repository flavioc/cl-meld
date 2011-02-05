(in-package :cl-meld)

(define-condition localize-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun get-routes (code)
   (mapfilter #'definition-name #L(has-elem-p (definition-options !1) :route) (definitions code)))
(defun get-paths (subgoals routes)
   (filter #L(has-test-elem-p routes (subgoal-name !1) #'string-equal) (get-subgoals subgoals)))
            
(defun select-subgoals-by-home (subgoals home-var)
   (filter #L(var-eq-p (first (subgoal-args !1)) home-var) (get-subgoals subgoals)))

(defun generate-args (n typs)
   (mapcar #L(list :var (concatenate 'string "X" (write-to-string !2)) !1) typs (enumerate 0 (1- n))))
(defun generate-inverse-name (name) (concatenate 'string "___" (reverse name)))
(defun swap-first-two-args (args) `(,(second args) ,(first args) ,@(rest (rest args))))   
(defun change-first-arg (args first) `(,first ,@(rest args)))

(defparameter *name-counter* 0)
(defun generate-mangled-name ()
   (with-output-to-string (a) (format a "__mangledname~a" (incf *name-counter*))))

(defun create-inverse-routes (code routes)
   (dolist (route routes)
      (let* ((new-name (generate-inverse-name route))
             (old-definition (lookup-definition (definitions code) route))
             (new-definition (make-definition new-name old-definition :route `(:reverse-route ,route)))
             (args (generate-args (length old-definition) old-definition))
             (new-clause (make-clause `(,(make-subgoal route args)) `(,(make-subgoal new-name (swap-first-two-args args))) :route)))
         (push new-clause (clauses code))
         (push new-definition (definitions code)))))
         
(defun select-valid-constraints (body vars) (filter #L(subsetp (all-variable-names !1) vars) (get-constraints body)))
(defun generate-inverse-subgoal (new-name to needed-vars)
   (make-subgoal new-name `(,to ,@needed-vars)))
                  
(defun match-paths (sources) #'(lambda (path) (some #L(equal !1 (first path)) sources)))
(defun expand-sources (sources paths) (mapfilter #'second (match-paths sources) paths))
(defun decrease-paths (sources paths) (remove-if (match-paths sources) paths))
(defun host-node (head) (first (subgoal-args (first head))))
                  
(defun check-valid-paths (paths0 host0)
   (let ((paths (mapcar #L(list (var-name (first (subgoal-args !1)))
                                (var-name (second (subgoal-args !1)))) paths0))
          (rm `(,(var-name host0))))
      (loop while paths
            for expand = (expand-sources rm paths)
            do (unless expand (error 'localize-invalid-error :text "Invalid paths in clause"))
               (format t "EXPANDED from ~A to ~A ~%" rm expand)
               (setf paths (decrease-paths rm paths))
               (setf rm expand))))
               
(defun variables-defined (body)
   (with-ret ret
      (do-assignments body (:var var) (push-dunion var ret))
      (do-subgoals body (:args args)
         (dolist (arg args)
            (when (var-p arg)
               (push-dunion arg ret))))))
               
(defun variables-undefined0 (head body)
   (with-ret ret
      (push-dunion-all (all-variables head) ret)
      (do-constraints body (:expr expr)
         (push-dunion-all (all-variables expr) ret))
      (do-subgoals body (:args args)
         (dolist (arg args)
            (unless (var-p arg)
               (push-dunion-all (all-variables arg) ret))))))       
(defun variables-undefined (head body)
   (set-tree-difference (variables-undefined0 head body) (variables-defined body)))

(defun valid-assignment-p (vars) #'(lambda (a) (tree-subsetp (all-variable-names (assignment-expr a)) vars)))
(defun select-valid-assignments (body subgoals)
   (let ((vars (all-variable-names subgoals))
         (ass (get-assignments body)))
      (with-ret ret
         (loop for next-assignments = (filter (valid-assignment-p vars) ass)
            while next-assignments
            do (progn
                  (setf ass (remove-if (valid-assignment-p vars) ass))
                  (push-all next-assignments ret)
                  (push-all (mapcar #L(var-name (assignment-var !1)) next-assignments) vars))))))
                  
(defun unneeded-assignment-p (body)
   #'(lambda (a)
         (let ((var-name (var-name (assignment-var a)))
               (vars (all-variable-names (remove-tree a body))))
            (not (has-elem-p vars var-name)))))
   
(defun remove-unneeded-assignments (body) ;; modifies body..
   (let ((ass (get-assignments body)))
      (loop for next-unneeded = (filter (unneeded-assignment-p body) ass)
            while next-unneeded
            do (progn
                  (setf ass (remove-all ass next-unneeded))
                  (setf body (remove-all body next-unneeded))))
      body))

(defun do-localize (code clause routes host)
   (let ((paths (get-paths (clause-body clause) routes)))
      (check-valid-paths paths host)
      (with-ret ret ;; ret stores new clauses
         (dolist (path paths)
            (let* ((subgoals (select-subgoals-by-home (clause-body clause) (second (subgoal-args path))))
                   (new-fact-name (generate-mangled-name)) (body (clause-body clause)) (head (clause-head clause))
                   (new-routing (make-subgoal (generate-inverse-name (subgoal-name path))
                                     (swap-first-two-args (subgoal-args path))))
                   (premisses `(,new-routing ,@subgoals))
                   (assignments (select-valid-assignments body subgoals))
                   (constraints (select-valid-constraints body (all-variable-names `(,@premisses ,@assignments))))
                   (stripped-body (remove-all body `(,path ,@subgoals ,@constraints)))
                   (everything-else `(,new-routing ,@stripped-body))
                   (new-clause-body (remove-unneeded-assignments `(,@subgoals ,new-routing ,@assignments ,@constraints)))
                   (variables-undef (variables-undefined head everything-else))
                   (variables-subgoals (variables-defined new-clause-body))
                   (needed-vars (tree-intersection variables-subgoals variables-undef))
                   (new-subgoal (generate-inverse-subgoal new-fact-name (first (subgoal-args path)) needed-vars)))
               (format t "~a~%" assignments)
               (setf (clause-body clause) (remove-unneeded-assignments `(,new-subgoal ,@stripped-body)))
               (push (make-definition (subgoal-name new-subgoal)
                           `(:type-node ,@(mapcar #'expr-type needed-vars)) `(:routed-tuple)) (definitions code))
               (push (make-clause new-clause-body `(,new-subgoal) :route) ret))))))
      
(defun localize-check-head (head)
   (let ((home (host-node head)))
      (do-subgoals (rest head) (:args args)
         (unless (var-eq-p (first args) home)
            (error 'localize-invalid-error
               :text "All head subgoals must have the same home argument")))))

(defun localize (code)
   (with-var new-clauses
      (let ((routes (get-routes code)))
         (do-clauses code (:clause clause :head head)
            (localize-check-head head)
            (push-all (do-localize code clause routes (host-node head)) new-clauses))
         (create-inverse-routes code routes)
         (push-all new-clauses (clauses code))))
   code)