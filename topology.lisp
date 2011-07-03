(in-package :cl-meld)

(defun node-used-id (mapping-set id)
   (gethash id mapping-set))
   
(defun add-mapping (mapping-set node map)
   (setf (gethash node mapping-set) map))
   
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

(defun find-edge-set (routes)
   (let ((hash (make-hash-table :test #'eq)))
      (do-axioms (:body body :head head)
         (when (and (null body) (link-head-p head routes))
            (let ((info (get-link-info head)))
               (add-edge-to-set hash info))))
      hash))
      
(defun empty-edge-set-p (edge-set) (zerop (hash-table-count edge-set)))
(defun filter-visited-nodes (ls node-set) (filter #L(in-hash-set-p node-set !1) ls))
         
(defun get-random-node-from-node-set (node-set)
   (do-hash-set (node node-set)
      (return-from get-random-node-from-node-set node)))
      
(defun bfs-ordering (edge-set node-set &optional (mapping-set (make-hash-table :test #'eq)) (queue (list)) (count 0))
   (cond
      ((empty-edge-set-p edge-set)
         (do-hash-set (node node-set) ; add remaining nodes
            (add-mapping mapping-set node count)
            (incf count))
         mapping-set)
      (t
         (setf queue (filter-visited-nodes queue node-set))
         (unless queue
            (push (get-random-node-from-node-set node-set) queue))
         (unless queue
            (return-from bfs-ordering mapping-set))
         (let* ((node (pop queue))
                (new-edges (get-neighbors-from-set edge-set node)))
            (add-mapping mapping-set node count)
            (remove-hash-set node-set node)
            (bfs-ordering edge-set node-set mapping-set (append queue new-edges) (1+ count))))))
            
(defun naive-ordering (nodes)
   (let ((hash (make-hash-table :test #'eq)))
      (loop for node in nodes
            for count = 0 then (1+ count)
            do (add-mapping hash node count))
      hash))

(defun random-ordering (nodes)
   (naive-ordering (shuffle-list nodes)))

(defun print-mapping (mapping-set)
   (iterate-nodes (fake real mapping-set)
      (format t "REAL: ~a FAKE: ~a~%" real fake)))

(defun do-topology-ordering ()
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
         