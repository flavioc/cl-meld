(in-package :cl-meld)

(defun valid-aggregate-p (agg)
   (let ((agg (aggregate-agg agg))
         (typ (aggregate-type agg)))
      (case agg
         (:first t)
         (:sum
            (eq-or typ :type-int :type-float :type-list-int :type-list-float))
         ((:min :max)
            (eq-or typ :type-int :type-float)))))
            
(defun update-aggregate-head (head body modifier edge-name agg-name get-fun)
   ;; If this rule produces an aggregate fact, push the route node into the last
   ;; argument or else put the home node (for local rules)
   (let ((head-subs (filter #L(equal (subgoal-name !1) agg-name) (get-subgoals head))))
      (when head-subs
         (let* ((host (first-host-node head))
                (routes (filter #L(equal (subgoal-name !1) edge-name) (get-subgoals body))))
            (if routes
               (setf host (funcall get-fun (subgoal-args (first routes))))
               (unless (aggregate-mod-includes-home-p modifier)
                  (aggregate-mod-include-home modifier)))
            (loop for sub in head-subs
                  do (push-end host (subgoal-args sub)))))))
         

(defun update-aggregate-input (modifier edge-name agg-name get-fun)
   "For an aggregate that has an INPUT/OUTPUT modifier, executes source code transformations
   that puts the input/output node as the last argument of the aggregate"
   (do-axioms (:head head)
      (update-aggregate-head head nil modifier edge-name agg-name get-fun))
   (do-rules (:head head :body body)
      (update-aggregate-head head body modifier edge-name agg-name get-fun)
      ;; Add an unnamed variable for clauses that use the aggregated result.
      (let ((body-subs (filter #L(equal (subgoal-name !1) agg-name) (get-subgoals body))))
         (loop for sub in body-subs
               do (push-end (generate-random-var) (subgoal-args sub)))))
   (let ((def (lookup-definition agg-name)))
      (assert (not (null def)))
      (push-end :type-addr (definition-types def))))

(defun valid-aggregate-modifier-p (agg-name agg)
   (let ((aggmod (aggregate-mod agg)))
      (cond
         ((null aggmod) t)
         ((aggregate-mod-is-immediate-p aggmod) t)
         ((aggregate-mod-is-input-p aggmod)
            (let* ((name (aggregate-mod-io-name aggmod))
                   (def (lookup-definition name))
                   (ret (and def (is-route-p def))))
               (when ret
                  (update-aggregate-input aggmod name agg-name #'first))
               ret))
         ((aggregate-mod-is-output-p aggmod)
            (let* ((name (aggregate-mod-io-name aggmod))
                   (def (lookup-definition name))
                   (ret (and def (is-route-p def))))
               (when ret
                  (update-aggregate-input aggmod name agg-name #'second))
               ret)))))

(defun check-aggregates (name typs)
   (let ((total (count-if #'aggregate-p typs)))
      (unless (<= total 1)
         (error 'type-invalid-error
            :text (concatenate 'string "tuple " name " must have only one aggregate")))
      (when-let ((agg (find-if #'aggregate-p typs)))
         (unless (valid-aggregate-p agg)
            (error 'type-invalid-error
               :text (tostring "invalid aggregate type: ~a" agg)))
         (unless (valid-aggregate-modifier-p name agg)
            (error 'type-invalid-error
               :text "invalid aggregate modifier")))))

(defun delete-from-body (clause construct)
   (setf (clause-body clause) (remove-tree-first construct (clause-body clause))))

(defparameter *agg-construct-counter* 0)
(defun generate-new-agg-pred ()
   (incf *agg-construct-counter*)
   (tostring "__agg_construct_~a" *agg-construct-counter*))
   
(defun unique-subgoal-in-head-p (clause)
   (with-clause clause (:head head :body body)
      (and  (null body)
            (not (null head))
            (= (length head) 1)
            (subgoal-p (first head)))))
         
(defun carry-total-subgoal-p (subgoal var)
   (with-subgoal subgoal (:args args)
      (when (>= (length args) 2)
         (var-eq-p var (second args)))))
         
(defun body-from-same-remote-p (body orig)
   (let (var)
      (do-subgoals body (:args args)
         (if (null var)
            (cond
               ((var-eq-p orig (first args))
                  (return-from body-from-same-remote-p nil))
               (t
                  (setf var (first args))))
            (unless (var-eq-p (first args) var)
               (return-from body-from-same-remote-p nil))))
      var))
      
(defun uses-route-subgoal-from-p (body host remote)
   (do-subgoals body (:name name :subgoal subgoal :args args)
      (when (and (>= (length args) 2)
               (var-eq-p (first args) remote)
               (var-eq-p (second args) host))
         (let ((def (lookup-definition name)))
            (when (is-route-p def)
               (return-from uses-route-subgoal-from-p subgoal)))))
      nil)
      
(defun get-agg-options-for-remote (body host to sub-name)
   (let ((rem-var (body-from-same-remote-p body host))
         (agg-options nil))
      (when rem-var
         (awhen (uses-route-subgoal-from-p body host rem-var)
            (setf agg-options `(:input ,(subgoal-name it)))
            (let* ((new-subgoal-0 (make-subgoal sub-name
                                 `(,host ,(make-int 0 (var-type to)))))
                (axiom (make-axiom `(,new-subgoal-0))))
            (push-end axiom *axioms*))))
      agg-options))

(defun transform-agg-construct (clause construct)
   (delete-from-body clause construct)
   (when (is-axiom-p clause)
      (with-agg-construct construct (:op op :to to :body body)
         (let ((def-name (generate-new-agg-pred))
               (var (clause-host-node clause)))
            (case op
               (:count
                  (cond
                     ((and (unique-subgoal-in-head-p clause)
                           (carry-total-subgoal-p (first (clause-head clause)) to))
                        (setf (clause-body clause) body)
                        (let* ((sub (first (clause-head clause)))
                               (def (lookup-subgoal-definition sub)))
                           (with-subgoal sub (:args args)
                              (setf (second args) (make-int 1 :type-int)))
                           (with-definition def (:types types)
                              (setf (second types) (make-aggregate :sum :type-int nil)))))
                     (t
                        (let* ((new-def (make-definition def-name `(:type-addr ,(make-aggregate :sum :type-int nil)) `(:linear)))
                               (new-subgoal (make-subgoal def-name `(,var ,to)))
                               (new-subgoal-head (make-subgoal def-name `(,var ,(make-int 1 :type-int))))
                               (new-clause (make-clause body `(,new-subgoal-head))))
                           (setf (clause-body clause) (cons new-subgoal (clause-body clause)))
                           (push-end new-clause *clauses*) 
                           (push-end new-def *definitions*)))))
               (:sum
                  (cond
                     ((and (unique-subgoal-in-head-p clause)
                           (carry-total-subgoal-p (first (clause-head clause)) to))
                        (setf (clause-body clause) body)
                        (let* ((sub (first (clause-head clause)))
                               (def (lookup-subgoal-definition sub))
                               (agg-options (get-agg-options-for-remote body var to (subgoal-name sub))))
                           (with-definition def (:types types)
                              (setf (second types) (make-aggregate :sum (second types) agg-options)))))
                     (t
                        (let* ((new-def (make-definition def-name `(:type-addr ,(make-aggregate :sum (var-type var) nil))))
                               (agg-options (get-agg-options-for-remote body var to def-name))
                               (new-subgoal (make-subgoal def-name `(:type-addr ,to)))
                               (new-clause (make-clause body `(,new-subgoal))))
                           (push-end new-clause *clauses*)
                           (push-end new-def *definitions*)))))
               (otherwise
                  (error 'aggtransformer-error :text (tostring "Aggregate ~a not recognized" op))))))))

(defun agg-transformer ()
   (do-rules (:clause clause :body body)
      (do-agg-constructs body (:agg-construct c)
         (transform-agg-construct clause c)))
   (do-definitions (:name name :types typs)
      (check-aggregates name typs)))
