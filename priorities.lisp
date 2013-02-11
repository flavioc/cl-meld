(in-package :cl-meld)

(defun make-descending-priority (a b) `(:prio ,a ,b))
(defun make-ascending-priority (a b) `(:prio ,b ,a))

(defun priority-p (x) (tagged-p x :prio))
(defun priority-left (x) (second x))
(defun priority-right (x) (third x))

(defun make-global-priority (pred-name argument asc-desc) `(:global-prio ,pred-name ,argument ,asc-desc))
(defun global-priority-p (x) (tagged-p x :global-prio))
(defun global-priority-name (x) (second x))
(defun global-priority-argument (x) (third x))
(defun global-priority-asc-desc (x) (fourth x))

(defun make-initial-priority (num) `(:initial-prio ,num))
(defun initial-priority-value (p) (second p))
(defun initial-priority-p (p) (tagged-p p :initial-prio))

(defun all-start-nodes (priorities)
	(remove-duplicates (mapcar #'priority-left priorities) :test #'string-equal))
(defun all-end-nodes (priorities)
	(remove-duplicates (mapcar #'priority-right priorities) :test #'string-equal))

(defun select-start-nodes (priorities)
	(set-difference (all-start-nodes priorities) (all-end-nodes priorities) :test #'string-equal))
			
(defun remove-all-start-nodes (priorities start-nodes)
	(split-mult-return #'(lambda (prio)
						(let* ((what (priority-left prio))
								 (x (find what start-nodes :test #'string-equal)))
							(if x t nil)))
			priorities))

(defun assign-priorities (base priorities)
	"Using the start priority 'base', it assigns increasing priorities taking into
account the dependencies between predicates."
	(let ((start-nodes (select-start-nodes priorities)))
		(cond
			((null start-nodes)
				(warn "COULD NOT ASSIGN PRIORITIES TO PREDICATES DUE TO CYCLES ~a" priorities)
				nil)
			(t
				(multiple-value-bind (removed remaining-nodes)
						(remove-all-start-nodes priorities start-nodes)
					(let* ((end-nodes (all-end-nodes removed))
							 (disappearing-nodes (set-difference end-nodes (all-start-nodes remaining-nodes) :test #'string-equal))
						 	 (result (mapcar #L(cons !1 base) start-nodes)))
						(append result
							(append (mapcar #L(cons !1 (1+ base)) disappearing-nodes)
								(if (null remaining-nodes) nil (assign-priorities (1+ base) remaining-nodes))))))))))
								
(defun find-priorities ()
	"Takes the *ast* code and finds new priorities from the comprehensions and aggregates."
   (return-from find-priorities nil)
	(do-rules (:head head :body body)
		(do-comprehensions head (:left left)
			(do-subgoals left (:name name1)
				(do-subgoals body (:name name2)
					(push-end (make-descending-priority name1 name2) *priorities*))))
		(do-agg-constructs head (:body aggbody)
			(do-subgoals aggbody (:name name1)
				(do-subgoals body (:name name2)
					(push-end (make-descending-priority name1 name2) *priorities*))))))
					
