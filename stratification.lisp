
(in-package :cl-meld)

(define-condition stratification-error (error)
   ((text :initarg :text :reader text)))
   
(defvar *strat-ctx* nil)
(defvar *strat-routes* nil)
(defvar *current-strat-level* 0)

(defun clause-edge-fact-p (routes)
   #L(let ((head-subgoal (first (clause-head !1))))
      (some #L(subgoal-matches-def-p head-subgoal !1) routes)))
      
(defun get-non-routes (defs)
   (remove-if #'is-route-p defs))

(defun make-stratification-ctx () (list))
(defun push-strata (def level)
   (definition-add-option def `(:strat ,level))
   (push def *strat-ctx*))
   
(defun subgoal-matches-any-def-p (sub &optional (ctx *strat-ctx*))
   (some #L(subgoal-matches-def-p sub !1) ctx))

(defun can-fire-clause-p (clause &optional (ctx *strat-ctx*))
   (when (every #'(lambda (sub) (subgoal-matches-any-def-p sub ctx))
            (get-subgoals (clause-body clause)))
      (clause-head clause)))

(defun is-aggregate-head-p (head)
   (when (one-elem-p head)
      (let ((subgoal (first head)))
         (let ((def (lookup-subgoal-definition subgoal)))
            (definition-aggregate-p def)))))
            
(defun is-aggregate-clause-p (clause)
   (let ((head (clause-head clause)))
      (is-aggregate-head-p head)))
      
(defun subgoal-not-generated-by-p (subgoal)
   #'(lambda (clause)
      (let ((head (clause-head clause)))
         (every #L(not (subgoal-match-p subgoal !1)) head))))
      
(defun heads-not-in-p (not-fire-clauses head)
   (every #L(every (subgoal-not-generated-by-p !1) not-fire-clauses) head))
   
(defun select-fired-rules (will-fire not-fire)
   (filter #'(lambda (fire-clause)
               (let ((head (clause-head fire-clause)))
                  (heads-not-in-p not-fire head)))
            will-fire))

(defun select-if-aggregate (clauses)
   (split-mult-return #'is-aggregate-clause-p clauses))
   
(defun get-head-subgoal (clause) (first (clause-head clause)))
(defun get-head-subgoal-name (clause) (subgoal-name (get-head-subgoal clause)))  
(defun lookup-subgoal-definition (subgoal) 
   (lookup-definition (definitions) (subgoal-name subgoal)))
                  
(defun group-clauses-by-head (clauses)
   (let ((hash (make-hash-table :test #'equal)))
      ;(format t "group clauses ~a~%" clauses)
      (loop for clause in clauses
            do (let ((name (get-head-subgoal-name clause)))
                  (multiple-value-bind (ls found-p) (gethash name hash)
                     (declare (ignore found-p))
                     ;(format t "ADD ~a~%" name)
                     (setf (gethash name hash)
                              (cons clause ls)))))
      (iterate-hash (hash name clauses :op collect) clauses)))

(defun local-clause-p (clause) (not (clause-has-tagged-option-p clause :route)))

(defun is-init-subgoal-p (subgoal) (is-init-p (lookup-subgoal-definition subgoal)))

(defun is-init-clause-p (clause)
   "Given a clause tells you if that clause is generated during init."
   (with-clause clause (:body body)
      (and (one-elem-p body)
           (is-init-subgoal-p (first body)))))

(defun body-generated-by-all-p (clause)
   (with-clause clause (:body body)
      (every #L(definition-has-option-p (lookup-subgoal-definition !1) :generated-by-all)
             (get-subgoals body))))
      
(defun clause-generated-by-all-p (clause)
   "Tells you if a clause will be fired by all nodes."
   (or (is-init-clause-p clause)
       (body-generated-by-all-p clause)))
   
(defun generated-by-all-p (clauses)
   (some #'clause-generated-by-all-p clauses))
   
(defun set-generated-by-all (def)
   (definition-add-option def :generated-by-all))

(defun is-unique-aggregate-p (def)
   "Tells if aggregate has only one value (that is, argument with aggregate is the first one)"
   (assert (definition-p def))
   (with-definition def (:types typs)
      (assert (>= (length typs) 1))
      (aggregate-p (first typs))))

(defun compute-clause-size (clause)
   (cond
      ((clause-has-tagged-option-p clause :route)
         (do-subgoals (clause-body clause) (:subgoal sub :name name)
            (let ((def (lookup-subgoal-definition sub)))
               (when (find def *strat-routes* :test #'equal)
                  (let ((inverted (generate-inverse-name name)))
                     (return-from compute-clause-size `(:remote ,inverted)))))))
      (t
         (let (ret)
            (with-clause clause (:body body)
               (do-subgoals body (:subgoal sub :operation collect)
                  (let ((def (lookup-subgoal-definition sub)))
                     (unless (is-init-p def)
                        (push (definition-name def) ret)))))
            (when ret
               `(:local ,@ret))))))
                        
(defun make-definition-size (locals remotes)
   (with-ret ret
      (setf remotes nil) ; XXX
      (when locals
         (setf ret `((:local ,@locals))))
      (when remotes
         (setf ret `((:remote ,@remotes) ,@ret)))))
         
(defun set-definition-size (def size) (definition-add-option def `(:size ,size)))
(defun definition-get-local-size (def)
   (let ((size (definition-get-tagged-option def :size)))
      (get-tagged-elem size :local)))
(defun definition-get-remote-size (def)
   (let ((size (definition-get-tagged-option def :size)))
      (get-tagged-elem size :remote)))
         
(defun compute-clauses-size (clauses &optional extra)
   (let (locals remotes)
      (when extra
         (push (definition-name extra) locals))
      (loop for clause in clauses
            for size = (compute-clause-size clause)
            do (unless (null size)
                  (cond
                     ((tagged-p size :local) (push-all (rest size) locals))
                     ((tagged-p size :remote) (push-all (rest size) remotes)))))
      (make-definition-size locals remotes)))
      
(defun process-unrecursive-aggs (agg-clauses)
   (loop for clauses in agg-clauses
         for def = (lookup-definition (definitions)
                        (get-head-subgoal-name (first clauses)))
         do (push-strata def *current-strat-level*)
         do (when (every #'local-clause-p clauses)
               (definition-add-option def `(:agg-type :local-agg)))
            (when (generated-by-all-p clauses)
                  (set-generated-by-all def))
            (let ((size (compute-clauses-size clauses def)))
               (set-definition-size def size)
               ;(format t "SIZE: ~a~%" size)
               )))
                  
(defun process-unrecursive-non-agg-clause (clause)
   (let ((by-all (clause-generated-by-all-p clause)))
      (with-clause clause (:head head)
         (do-subgoals head (:subgoal sub)
            (let ((def (lookup-subgoal-definition sub)))
               (push-strata def *current-strat-level*)
               (if by-all
                  (set-generated-by-all def)))))))

(defun process-unrecursive-non-aggs (clauses)
   (loop for clause in clauses
         do (process-unrecursive-non-agg-clause clause)))
         
(defun get-head-definitions (clauses)
   (with-ret defs
      (do-clauses clauses (:head head)
         (do-subgoals head (:subgoal sub)
            (push (lookup-subgoal-definition sub) defs)))))
            
(defun definition-has-stage-argument (def)
   (with-definition def (:types typs)
      (and (>= (length typs) 1)
           (type-int-p (first typs)))))
           
(defun clause-has-initial-stage-argument (clause)
   (with-clause clause (:head head)
      (every #'(lambda (sub)
               (with-subgoal sub (:args args)
                  (and (>= (length args) 1)
                       (int-p (first args)))))
         (get-subgoals head))))
         
(defun has-every-definition-p (subgoals defs)
   (every #'(lambda (sub)
               (some #L(subgoal-matches-def-p sub !1) defs))
            subgoals))
         
(defun find-end-clique-clauses (defs all-clauses start-clauses)
   (remove-all (filter #'(lambda (clause)
                  (with-clause clause (:head head)
                     (has-every-definition-p (get-subgoals head) defs)))
               all-clauses)
            start-clauses))
            
(defun find-subgoals-by-definitions (body-head defs)
   (filter #'(lambda (sub)
               (some #L(subgoal-matches-def-p sub !1) defs))
         (get-subgoals body-head)))
         
(defun current-strat-iteration-used (body initial-defs)
   (let ((subs (find-subgoals-by-definitions body initial-defs)))
      (when (every #'(lambda (sub)
                        (with-subgoal sub (:args args)
                           (and (>= (length args) 1)
                              (var-p (first args)))))
                     subs)
         (let ((sub (first subs)))
            (first (subgoal-args sub))))))
      
(defun next-strat-iteration-is-used-p (head needed-defs iter)
   (let ((subs (find-subgoals-by-definitions head needed-defs)))
      (and (not (null subs))
           (every #'(lambda (sub)
                        (with-subgoal sub (:args args)
                           (and (>= (length args) 1)
                              (let ((arg (first args)))
                                    (and (op-p arg)
                                         (let ((op1 (op-op1 arg))
                                               (op2 (op-op2 arg)))
                                          (and (var-eq-p op1 iter)
                                             (int-p op2)
                                             (> (int-val op2) 0))))))))
                  subs))))
         
            
(defun find-strat-dependent-clauses (clauses initial-defs needed-defs)
   (with-ret new-clauses
      (do-clauses clauses (:body body :head head :clause clause)
         (when (can-fire-clause-p clause (append initial-defs *strat-ctx*))
            (let ((iter (current-strat-iteration-used body initial-defs)))
               (when (and iter
                        (next-strat-iteration-is-used-p head needed-defs iter))
               (push clause new-clauses)))))))
            
(defun find-needed-defs (remain-clauses initial-defs)
   (with-ret new-defs
      (do-clauses remain-clauses (:body body)
         (do-subgoals body (:subgoal sub)
            (unless (subgoal-matches-any-def-p sub (append initial-defs *strat-ctx*))
               (push (lookup-subgoal-definition sub) new-defs))))))

(defun mark-strat-clauses-to-delete (clauses defs)
   (loop for clause in clauses
         for stage = (make-minus (first (subgoal-args (first (clause-head clause)))) '- (make-forced-int 1))
         do (clause-add-option clause
               `(:delete ,@(loop for def in defs collect `(,(definition-name def) ,stage))))))
               
(defun find-clause-clique (start-clauses clauses)
   (let* ((initial-defs (get-head-definitions start-clauses))
          (has-stage-first (every #'definition-has-stage-argument initial-defs))
          (has-stage-args-first (every #'clause-has-initial-stage-argument start-clauses)))
      (cond
         ((not (and has-stage-first has-stage-args-first))
            (format t "Could not find a valid stratified clique~%")
            nil)
         (t
            (let ((remain-clauses (find-end-clique-clauses initial-defs clauses start-clauses)))
               (when remain-clauses
                  (let* ((needed-defs (find-needed-defs remain-clauses initial-defs))
                         (needed-clauses (find-strat-dependent-clauses clauses initial-defs needed-defs)))
                     (unless (every #'definition-aggregate-p needed-defs)
                      (return-from find-clause-clique nil))
                     (loop for def in initial-defs 
                           do (set-generated-by-all def))
                     (let ((size (compute-clauses-size needed-clauses)))
                        (loop for def in needed-defs
                              do (set-definition-size def size)))
                     (when (generated-by-all-p needed-clauses)
                        (loop for def in needed-defs
                              do (set-generated-by-all def)))
                     (loop for def in initial-defs
                           do (push-strata def *current-strat-level*))
                     (incf *current-strat-level*)
                     (loop for def in needed-defs
                           do (push-strata def *current-strat-level*))
                     (mark-strat-clauses-to-delete remain-clauses (append initial-defs needed-defs))
                     `(,@start-clauses ,@remain-clauses ,@needed-clauses))))))))

(defun stratification-loop (clauses)
   (multiple-value-bind (will-fire not-fire) (split-mult-return #'can-fire-clause-p clauses)
      (let ((will-really-fire (select-fired-rules will-fire not-fire)))
         (if (null will-really-fire)
            (progn
               (let ((clique (find-clause-clique will-fire clauses)))
                  (remove-all clauses clique)))
            (multiple-value-bind (agg-clauses not-agg) (select-if-aggregate will-really-fire)
               (when agg-clauses
                  (let ((grouped-agg (group-clauses-by-head agg-clauses)))
                     (process-unrecursive-aggs grouped-agg)))
               (process-unrecursive-non-aggs not-agg)
               (remove-all clauses will-really-fire))))))

(defun mark-unstratified-predicates ()
   (dolist (def (definitions))
      (unless (definition-has-tagged-option-p def :strat)
         (with-definition def (:name name)
            ;; XXX: remove
            (when (equal name "receive")
               (set-definition-size def 
                  (make-definition-size nil (list (generate-inverse-name "link")))))
            (when (equal name "allhiddengrad")
               (set-definition-size def
                  (make-definition-size nil (list "link"))))
            (push-strata def *current-strat-level*)))))

(defun do-strat-loop (clauses)
   (incf *current-strat-level*)
   (when (null clauses)
      (format t "Everything stratified!~%")
      (mark-unstratified-predicates)
      (return-from do-strat-loop nil))
   (let ((remain (stratification-loop clauses)))
      (cond
         ((equal remain clauses)
            (warn "Could not stratify everything")
            (mark-unstratified-predicates))
         (t
            (do-strat-loop remain)))))
   
(defun stratify ()
   (let* ((*strat-routes* (get-routes))
          (*current-strat-level* 0)
          (*strat-ctx* (make-stratification-ctx))
          (clauses (remove-if (clause-edge-fact-p *strat-routes*) (clauses))))
      (dolist (rout *strat-routes*)
         (push-strata rout *current-strat-level*))
      (let ((init-def (find-init-predicate (definitions))))
         (set-generated-by-all init-def)
         (push-strata init-def *current-strat-level*))
      (if *use-stratification*
         (do-strat-loop clauses)
         (progn
            (incf *current-strat-level*)
            (mark-unstratified-predicates)))))
