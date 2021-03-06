(in-package :cl-meld)

(defun make-allocator (typ)
	(let* ((remain1 (subseq typ 1))
			 (remain (subseq remain1 0 (1- (length remain1)))))
    (let ((vec (split-string remain :delimiterp #'(lambda (x) (char= x #\Space))))
          opts)
     (if (find-if #'(lambda (a) (string-equal a "basic")) vec)
      (push :basic opts))
     `(:allocator ,opts))))
(defun allocator-has-option-p (alloc opt)
   (find opt (second alloc)))
(defun find-allocator ()
   (find-if #L(tagged-p !1 :allocator) *directives*))

(defun make-index (name field) `(:index ,name ,field))
(defun index-name (x) (second x))
(defun index-field (x) (third x))
(defun index-p (x) (tagged-p x :index))
(defun find-index-name (name)
 (when *use-index*
    (find-if #L(and (index-p !1) (string-equal name (index-name !1))) *directives*)))

(defun make-compact (name) `(:compact ,name))
(defun compact-name (x) (second x))
(defun compact-p (x) (tagged-p x :compact))
(defun find-compact-name (name)
 (when *use-compact*
  (or (let ((def (lookup-definition name)))
         (definition-is-compact-p def))
     (find-if #L(and (compact-p !1) (string-equal name (compact-name !1))) *directives*))))

(defun make-data-input (template file &optional args) `(:data-input ,template ,file ,args))
(defun data-input-template (x) (second x))
(defun data-input-file (x) (third x))
(defun data-input-args (x) (fourth x))
(defun data-input-p (x) (tagged-p x :data-input))
(defun find-data-input () (find-if #L(data-input-p !1) *directives*))

(defun make-descending-priority (a b) `(:prio ,a ,b))
(defun make-ascending-priority (a b) `(:prio ,b ,a))

(defun priority-p (x) (tagged-p x :prio))
(defun priority-left (x) (second x))
(defun priority-right (x) (third x))

(defun make-initial-priority (num) `(:initial-prio ,num))
(defun initial-priority-value (p) (second p))
(defun initial-priority-p (p) (tagged-p p :initial-prio))

(defun make-default-priority (num) `(:default-prio ,num))
(defun default-priority-p (p) (tagged-p p :default-prio))
(defun default-priority-value (p) (second p))

(defun make-no-priority (num) `(:no-priority ,num))
(defun no-priority-p (p) (tagged-p p :no-priority))
(defun no-priority-value (p) (second p))

(defun make-priority-no-initial () `(:no-initial-priorities))
(defun priority-no-initial-p (p) (tagged-p p :no-initial-priorities))

(defun make-priority-order (asc-desc) `(:priority-order ,asc-desc))
(defun priority-order (x) (second x))
(defun priority-order-p (x) (tagged-p x :priority-order))

(defun make-priority-static () `(:priority-static))
(defun priority-static-p (x) (tagged-p x :priority-static))

(defun make-priority-cluster (typ) `(:priority-cluster ,typ))
(defun priority-cluster-type (x) (second x))
(defun priority-cluster-p (x) (tagged-p x :priority-cluster))

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

(defun get-priority-static () (find-if #'priority-static-p *directives*))

(defun get-priority-order ()
	"Returns priority ordering for the program."
	(let ((order (find-if #'priority-order-p *directives*)))
		(if order
			(priority-order order)
			:desc)))

(defun get-default-priority ()
	"Returns default priority for nodes of the program."
   (let ((found (find-if #'default-priority-p *directives*)))
    (if found
     (default-priority-value found)
     (case (get-priority-order)
		(:asc most-positive-double-float)
      (:desc most-negative-double-float)))))

(defun get-initial-priority ()
	"Returns initial priority for nodes of the program."
	(let ((found (find-if #'initial-priority-p *directives*)))
		(if found
			(initial-priority-value found)
         (let ((no-initial (find-if #'priority-no-initial-p *directives*)))
          (if no-initial
            (get-default-priority)
            (case (get-priority-order)
             (:asc most-negative-double-float)
             (:desc most-positive-double-float)))))))

(defun get-no-priority-value ()
   "Returns the value that represents base priorities."
   (let ((found (find-if #'no-priority-p *directives*)))
    (if found
     (no-priority-value found)
     (case (get-priority-order)
      (:asc most-positive-double-float)
      (:desc most-negative-double-float)))))

(defun assign-priorities (base priorities)
	"Using the start priority 'base', it assigns increasing priorities taking into
account the dependencies between predicates."
	(let ((start-nodes (select-start-nodes priorities)))
		(cond
			((null start-nodes) nil)
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
					(push-end (make-descending-priority name1 name2) *directives*))))
		(do-agg-constructs head (:body aggbody)
			(do-subgoals aggbody (:name name1)
				(do-subgoals body (:name name2)
					(push-end (make-descending-priority name1 name2) *directives*))))))
					
(defgeneric data-input-node-axioms (data n))
(defgeneric data-input-nodes (data))
