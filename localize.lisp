(in-package :cl-meld)

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

(defun generate-inverse-subgoal (subgoal to)
   (make-subgoal (generate-inverse-name (subgoal-name subgoal))
                  (change-first-arg (subgoal-args subgoal) to)))

(defun do-localize (code clause routes)
   (with-var ret
      (dolist (path (get-paths (clause-body clause) routes))
         (let* ((subgoals (select-subgoals-by-home (clause-body clause) (second (subgoal-args path)))))
            (do-subgoals subgoals (:orig sub)
               (let* ((new-routing (make-subgoal (generate-inverse-name (subgoal-name path))
                                       (swap-first-two-args (subgoal-args path))))
                     (to (first (subgoal-args path)))
                     (premisses0 (list new-routing sub))
                     (constraints (select-valid-constraints (clause-body clause) (all-variable-names premisses0)))
                     (inverse-sub (generate-inverse-subgoal sub to)))
                  (push (make-definition (subgoal-name inverse-sub)
                                          (lookup-definition (definitions code) (subgoal-name sub)) `(:inverse-tuple ,(subgoal-name sub))) (definitions code))
                  (push (make-clause `(,sub ,new-routing ,@constraints) `(,inverse-sub) :route) ret)
                  (setf (clause-body clause)
                     `(,inverse-sub ,@(remove-all (clause-body clause) `(,sub ,path ,@constraints))))))))
      ret))

(defun localize (code)
   (with-var new-clauses
      (let ((routes (get-routes code)))
         (do-clauses code (:clause clause)
            (push-all (do-localize code clause routes) new-clauses) )
         (create-inverse-routes code routes)
         (push-all new-clauses (clauses code))))
   code)
   