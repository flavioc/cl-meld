(in-package :cl-meld)

(defun node-used-id (mapping-set id)
   (gethash id mapping-set))
   
(defun add-mapping (mapping-set node map)
   (setf (gethash node mapping-set) map))
   
(defun make-mapping-set ()
   (make-hash-table :test #'eq))
   
(defun number-of-nodes (nodes) (hash-table-count nodes))

(defmacro iterate-nodes ((fake real nodes) &body body)
   `(iterate-hash (,nodes ,real ,fake) ,@body))

(defun flip-nodes (hash expr)
   (transform-expr #'addr-p #'(lambda (expr)
                                 (setf (addr-num expr) (node-used-id hash (addr-num expr)))
                                 (values nil :stop))
                  expr))
                  
(defun link-head-p (head routes)
   (and (one-elem-p head)
        (with-subgoal (first head) (:name name :args args)
            (and (some #L(equal name !1) routes)
                 (= (length args) 2)
                 (addr-p (first args))
                 (addr-p (second args))))))
                 
(defun get-link-info (head)
   (with-subgoal (first head) (:args args)
      (list (addr-num (first args))
            (addr-num (second args)))))
            
(defun add-edge-to-set (hash info)
   (let ((from (first info))
         (to (second info)))
      ;(format t "from ~a to ~a~%" from to)
      (setf (gethash from hash) (cons to (gethash from hash)))))
      
(defun get-neighbors-from-set (hash from)
   (multiple-value-bind (ls found-p) (gethash from hash)
      (when found-p
         (remhash from hash)
         ls)))
         
(defun make-edge-set ()
   (make-hash-table :test #'eq))

(defun find-edge-set (routes)
   (letret (hash (make-edge-set))
      (do-axioms (:body body :head head)
         (when (and (null body) (link-head-p head routes))
            (let ((info (get-link-info head)))
               (add-edge-to-set hash info))))))
      
(defun empty-edge-set-p (edge-set) (zerop (hash-table-count edge-set)))
(defun filter-visited-nodes (ls node-set) (filter #L(in-hash-set-p node-set !1) ls))
         
(defun get-random-node-from-node-set (node-set)
   (do-hash-set (node node-set)
      (return-from get-random-node-from-node-set node)))
      
(defun aux-graph-has-cycles-p (edge-set node-set &optional (queue (list)))
   (when (find-if #L(not (in-hash-set-p node-set !1)) queue)
      (return-from aux-graph-has-cycles-p t))
   (cond
      ((and (empty-edge-set-p edge-set)
            (null queue))
         nil)
      (t
         (unless queue
            (push (get-random-node-from-node-set node-set) queue))
         (unless queue
            (return-from aux-graph-has-cycles-p nil))
         (let* ((node (pop queue))
                (new-edges (get-neighbors-from-set edge-set node)))
            (remove-hash-set node-set node)
            (aux-graph-has-cycles-p edge-set node-set (append queue new-edges))))))

(defun graph-has-cycles-p ()
   (aux-graph-has-cycles-p (find-edge-set (get-route-names)) (create-hash-set *nodes*)))

(defun bfs-ordering (edge-set node-set &key (mapping-set (make-mapping-set)) (queue (list)) (count 0))
   (cond
      ((empty-edge-set-p edge-set)
         (do-hash-set (node node-set) ; add remaining nodes
            (add-mapping mapping-set node count)
            (incf count))
         mapping-set)
      (t
         (setf queue (filter-visited-nodes queue node-set))
         (unless queue
            (unless (zerop (hash-table-count node-set))
               (push (get-random-node-from-node-set node-set) queue)))
         (unless queue
            (return-from bfs-ordering mapping-set))
         (let* ((node (pop queue))
                (new-edges (get-neighbors-from-set edge-set node)))
            (add-mapping mapping-set node count)
            (remove-hash-set node-set node)
            (bfs-ordering edge-set node-set
                           :mapping-set mapping-set
                           :queue (append queue new-edges)
                           :count (1+ count))))))
            
(defun naive-ordering (nodes &key (start-count 0) (mapping (make-mapping-set)))
   (loop for node in nodes
         for count = start-count then (1+ count)
         do (add-mapping mapping node count))
   mapping)

(defun random-ordering (nodes &key (start-count 0) (mapping (make-mapping-set)))
   (naive-ordering (shuffle-list nodes) :start-count start-count :mapping mapping))

(defun print-mapping (mapping-set)
   (iterate-nodes (fake real mapping-set)
      (format t "REAL: ~a FAKE: ~a~%" real fake)))
      
(defun is-constant-node-list-p (ls)
   (cond
      ((nil-p ls) t)
      ((cons-p ls)
         (and (addr-p (cons-head ls))
            (is-constant-node-list-p (cons-tail ls))))))

(defun get-constant-list-addrs (ls)
   (cond
      ((nil-p ls) nil)
      ((cons-p ls)
         (cons (addr-num (cons-head ls))
               (get-constant-list-addrs (cons-tail ls))))
      (t (assert nil))))
   
(defun has-select-nodes-p ()
   (let ((ls (filter #L(subgoal-appears-code-p !1 "select_nodes") *worker-axioms*)))
      (and (one-elem-p ls)
           (with-subgoal (first (clause-head (first ls))) (:args args)
               (and (var-p (first args))
                    (is-constant-node-list-p (second args)))))))
      
(defun build-initial-mapping-set (addrs total)
   (letret (mapping (make-mapping-set))
      (loop for i from 0 upto (1- total)
            for node in addrs
            do (add-mapping mapping node i))))
            
(defun programmer-ordering (nodes)
   (let* ((clause (find-if #L(subgoal-appears-code-p !1 "select_nodes") *worker-axioms*))
          (sub (first (clause-head clause))))
      (assert sub)
      (with-subgoal sub (:args args)
         (let* ((addrs (get-constant-list-addrs (second args)))
                (total (length addrs))
                (mapping-set (build-initial-mapping-set addrs total))
                (remaining-nodes (set-difference nodes addrs)))
            (when remaining-nodes
               (warn "Not all nodes are present in select_nodes"))
            (case *ordering-type*
               (:naive (naive-ordering remaining-nodes :start-count total :mapping mapping-set))
               (:random (random-ordering remaining-nodes :start-count total :mapping mapping-set))
               (:breadth (let ((remaining-node-set (create-hash-set remaining-nodes))
                               (edge-set (if (null remaining-nodes)
                                             (make-edge-set)
                                             (find-edge-set (get-route-names)))))
                           (bfs-ordering edge-set remaining-node-set
                                          :mapping-set mapping-set
                                          :count total))))))))

(defun do-topology-ordering ()
   (when (has-select-nodes-p)
      (let ((mapping (programmer-ordering *nodes*)))
         ;; Remove select_nodes definition
         ;; (setf *definitions* (remove-if #L(equal (definition-name !1) "select_nodes") *definitions*))
      (return-from do-topology-ordering mapping)))
   (case *ordering-type*
      (:naive (naive-ordering *nodes*))
      (:random (random-ordering *nodes*))
      (:breadth (let ((edge-set (find-edge-set (get-route-names)))
                      (node-set (create-hash-set *nodes*)))
                  (bfs-ordering edge-set node-set)))
      (otherwise (assert nil))))

(defun optimize-topology ()
   (let ((mapping (do-topology-ordering)))
      ;(print-mapping mapping)
      (setf *nodes* mapping)
      (do-axioms (:clause clause)
         (flip-nodes mapping clause))))
