(in-package :cl-meld)

(define-condition localize-invalid-error (error)
   ((text :initarg :text :reader text)))
   
(defvar *route-facts-to-invert* nil)
(defun add-route-fact-to-invert (fact) (push-dunion fact *route-facts-to-invert*))

(defun get-first-arg (subgoal) (first (subgoal-args subgoal)))
(defun get-second-arg (subgoal) (second (subgoal-args subgoal)))

(defun get-paths (subgoals routes)
   (filter #L(has-test-elem-p routes (subgoal-name !1) #'string-equal) (get-subgoals subgoals)))

(defun equal-to-any-home (arg homes) (some #L(var-eq-p arg !1) homes))
(defun select-subgoals-by-home (subgoals home-vars)
   (filter #L(equal-to-any-home (get-first-arg !1) home-vars) (get-subgoals subgoals)))

(defun generate-args (n typs)
   (mapcar #L(make-var (concatenate 'string "X" (write-to-string !2)) !1) typs (enumerate 0 (1- n))))
(defun generate-inverse-name (name) (concatenate 'string "___" (reverse name)))
(defun swap-first-two-args (args) `(,(second args) ,(first args) ,@(rest (rest args))))   
(defun change-first-arg (args first) `(,first ,@(rest args)))

(defparameter *name-counter* 0)
(defun generate-mangled-name ()
   (with-output-to-string (a) (format a "__mangledname~a" (incf *name-counter*))))

(defun create-inverse-routes (code)
   (dolist (route *route-facts-to-invert*)
      (let* ((new-name (generate-inverse-name route))
             (old-definition (lookup-definition-types (definitions code) route))
             (new-definition (make-definition new-name old-definition :route `(:reverse-route ,route)))
             (args (generate-args (length old-definition) old-definition))
             (new-clause (make-clause `(,(make-subgoal route args))
                     `(,(make-subgoal new-name (swap-first-two-args args))) `(:route ,(var-name (second args))))))
         (push new-clause (clauses code))
         (push new-definition (all-definitions code)))))
         
(defun select-valid-constraints (body vars) (filter #L(subsetp (all-variable-names !1) vars) (get-constraints body)))
(defun generate-inverse-subgoal (new-name to needed-vars)
   (make-subgoal new-name `(,to ,@needed-vars)))

(defun match-paths (sources part) #'(lambda (path) (some #L(var-eq-p !1 (funcall part path)) sources)))
(defun expand-sources (sources paths)
   (append (mapfilter #'second (match-paths sources #'first) paths)
           (mapfilter #'first (match-paths sources #'second) paths)))
(defun decrease-paths (sources paths)
   (remove-if (match-paths sources #'second)
              (remove-if (match-paths sources #'first) paths)))
(defun host-node (head) (get-first-arg (first head)))
                  
(defun get-reachable-nodes (paths-sub start-node)
   (let ((paths (mapcar #L(list (get-first-arg !1)
                                (get-second-arg !1)) paths-sub))
          (rm `(,start-node)))
      (loop while paths
            for expand = (expand-sources rm paths)
            do (unless expand
                  (error 'localize-invalid-error :text (tostring "Invalid paths in clause: ~a" paths-sub)))
               (setf paths (decrease-paths rm paths)
                     rm (append rm expand)))
      rm))
               
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

(defun get-inverse-route (route-subgoal)
   (make-subgoal (generate-inverse-name (subgoal-name route-subgoal))
                (swap-first-two-args (subgoal-args route-subgoal))))

(defun do-localize-one (code clause from to route-subgoal remaining &optional (order 'forward))
   (let* ((reachable (get-reachable-nodes remaining to))
          (subgoals (select-subgoals-by-home (clause-body clause) reachable)))
      (unless subgoals
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
               (clause-add-option clause `(:route ,(var-name from)))
               (values clause nil))
            ((and (eq order 'forward)
                  (null stripped-body))
               (add-route-fact-to-invert (subgoal-name route-subgoal))
               (setf stripped-body `(,new-routing ,@assignments ,@constraints ,@subgoals))
               (clause-add-option clause `(:route ,(var-name from)))
               (setf (clause-body clause) stripped-body)
               (values clause nil))
            (t
               (when new-routing
                  (add-route-fact-to-invert (subgoal-name route-subgoal))
                  (push new-routing subgoals))
               (let* ((new-clause-body `(,@subgoals ,@assignments ,@constraints))
                      (variables-undef (variables-undefined head (if (eq order 'forward) `(,new-routing ,@stripped-body)
                                                                     stripped-body)))
                      (variables-subgoals (variables-defined new-clause-body))
                      (needed-vars (tree-intersection variables-subgoals variables-undef))
                      (new-subgoal (generate-inverse-subgoal (generate-mangled-name) (get-first-arg route-subgoal) needed-vars)))
                  (setf (clause-body clause) (remove-unneeded-assignments `(,new-subgoal ,@stripped-body) head))
                  (with-subgoal new-subgoal (:name name)
                     (push (make-definition name `(:type-addr ,@(mapcar #'expr-type needed-vars)) `(:routed-tuple))
                           (all-definitions code)))
                  (let* ((new-clause-head `(,(copy-tree new-subgoal)))
                         (new-clause-body (remove-unneeded-assignments new-clause-body new-clause-head))
                         (route-to (var-name from)))
                     (values (make-clause new-clause-body new-clause-head `(:route ,route-to))
                              t))))))))
            
(defun get-direction-and-dest (host edge)
   (if (var-eq-p host (get-first-arg edge))
       (values 'forward (get-second-arg edge))
       (values 'backward (get-first-arg edge))))
          
(defun do-localize (host code clause edges remaining)
   "From node HOST in clause CLAUSE localize from EDGES"
   (dolist (edge edges)
      (multiple-value-bind (order to) (get-direction-and-dest host edge)
         (let* ((fun (edges-equal-to to))
                (new-edges (filter fun remaining))
                (new-remaining (remove-if fun remaining)))
            (multiple-value-bind (target-clause add-to-program-p)
                                    (do-localize-one code clause host to edge remaining order)
               (when target-clause
                  (if add-to-program-p
                     (push target-clause (clauses code)))
                  (when new-edges
                     (do-localize to code target-clause new-edges new-remaining))))))))

(defun check-subgoal-arguments (homes clause)
   (with-clause clause (:body body :head head)
      (do-subgoals (append body head) (:args args :name name)
         (unless (some #'(lambda (h) (var-eq-p (first args) h)) homes)
            (error 'localize-invalid-error
                  :text (tostring "Subgoal ~a has a bad home argument: ~a" name (first args)))))))
   
(defun edges-equal-to (host) #L(or (var-eq-p host (get-first-arg !1)) (var-eq-p host (get-second-arg !1))))
(defun localize-start (code clause routes host)
   (let ((paths (get-paths (clause-body clause) routes)))
      (unless paths
         (return-from localize-start t)) ; no localization needed
      (let ((home-arguments (get-reachable-nodes paths host)))
         (check-subgoal-arguments home-arguments clause)
         (let* ((fun (edges-equal-to host))
                (edges (filter fun paths))
                (remaining (remove-if fun paths)))
            (do-localize host code clause edges remaining)))))
      
(defun localize-check-head (head)
   (let ((home (host-node head)))
      (do-subgoals (rest head) (:args args)
         (unless (var-eq-p (first args) home)
            (error 'localize-invalid-error
               :text "All head subgoals must have the same home argument")))))

(defun remove-home-argument (code)
   (do-clauses (clauses code) (:head head :body body)
      (let (head-var (host-id (make-host-id)))
         (do-subgoals (append body head)
                     (:args args :orig sub)
            (unless head-var (setf head-var (first args)))
            (setf (subgoal-args sub) (rest args)))
         (when head ; change home argument to host-id
            (nsubst host-id head-var head :test #'equal)
            (nsubst host-id head-var body :test #'equal)))) 
   (do-definitions code (:definition def :types typs)
      (setf (definition-types def) (rest typs))))

(defun localize (code)
   (let ((routes (get-routes code))
         (*route-facts-to-invert* nil))
      (do-clauses (clauses code) (:clause clause :head head)
         (localize-check-head head)
         (localize-start code clause routes (host-node head)))
      (create-inverse-routes code))
   (remove-home-argument code)
   code)
