(in-package :cl-meld)

(defun delete-from-body (clause construct)
   (setf (clause-body clause) (remove-tree-first construct (clause-body clause))))

(defparameter *agg-construct-counter* 0)
(defun generate-new-agg-pred ()
   (incf *agg-construct-counter*)
   (tostring "__agg_construct_~a" *agg-construct-counter*))
   
(defun unique-subgoal-p (head)
   (and (not (null head))
         (= (length head) 1)
         (subgoal-p (first head))))
         
(defun carry-total-subgoal-p (subgoal var)
   (with-subgoal subgoal (:args args)
      (when (>= (length args) 2)
         (var-eq-p var (second args)))))

(defun transform-agg-construct (clause construct)
   (delete-from-body clause construct)
   (when (is-axiom-p clause)
      (with-agg-construct construct (:op op :to to :body body)
         (let ((def-name (generate-new-agg-pred))
               (var (clause-host-node clause)))
            (case op
               (:count
                  (cond
                     ((and (unique-subgoal-p (clause-head clause))
                           (carry-total-subgoal-p (first (clause-head clause)) to))
                        (setf (clause-body clause) body)
                        (let* ((sub (first (clause-head clause)))
                               (def (lookup-subgoal-definition sub)))
                           (with-subgoal sub (:args args)
                              (setf (second args) (make-int 1 :type-int)))
                           (with-definition def (:types types)
                              (setf (second types) (make-aggregate :sum :type-int nil)))))
                     (t
                        (let* ((new-def (make-definition def-name `(:type-node ,(make-aggregate :sum :type-int nil)) `(:linear)))
                               (new-subgoal (make-subgoal def-name `(,var ,to)))
                               (new-subgoal-head (make-subgoal def-name `(,var ,(make-int 1 :type-int))))
                               (new-clause (make-clause body `(,new-subgoal-head))))
                           (setf (clause-body clause) (cons new-subgoal (clause-body clause)))
                           (push-end new-clause *clauses*) 
                           (push-end new-def *definitions*))))))))))

(defun agg-transformer ()
   (do-rules (:clause clause :body body)
      (do-agg-constructs body (:agg-construct c)
         (transform-agg-construct clause c))))
   