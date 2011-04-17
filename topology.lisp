(in-package :cl-meld)

(define-condition expr-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun node-used-id (mapping-set id)
   (gethash id mapping-set))
   
(defun add-mapping (mapping-set node map)
   (setf (gethash node mapping-set) map))
   
(defun number-of-nodes (nodes) (hash-table-count nodes))

(defmacro iterate-nodes ((fake real nodes) &body body)
   `(loop for ,real being the hash-keys of ,nodes
          using (hash-value ,fake)
          do (progn ,@body)))
          
(defun flip-nodes (hash expr)
   (iterate-expr #'(lambda (expr)
                     (if (addr-p expr)
                        (setf (addr-num expr) (node-used-id hash (addr-num expr)))))
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
      (format t "from ~a to ~a~%" from to)
      (setf (gethash from hash) (cons to (gethash from hash)))))
      
(defun get-neighbors-from-set (hash from)
   (multiple-value-bind (ls found-p) (gethash from hash)
      (when found-p
         (remhash from hash)
         ls)))

(defun find-edge-set (ast routes)
   (let ((hash (make-hash-table :test #'eq)))
      (do-clauses (clauses ast) (:body body :head head)
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
            (format t "FAIL~%")
            (return-from bfs-ordering mapping-set))
         (let* ((node (pop queue))
                (new-edges (get-neighbors-from-set edge-set node)))
            (format t "ADD ~a(~a) QUEUE ~a~%" node count (append queue new-edges))
            (add-mapping mapping-set node count)
            (remove-hash-set node-set node)
            (bfs-ordering edge-set node-set mapping-set (append queue new-edges) (1+ count))))))
            
(defun naive-ordering (nodes) ; (defined-nodes ast)
   (let ((hash (make-hash-table :test #'eq)))
      (loop for node in nodes
            for count = 0 then (1+ count)
            do (add-mapping hash node count))
      hash))

(defun print-mapping (mapping-set)
   (iterate-nodes (fake real mapping-set)
      (format t "REAL: ~a FAKE: ~a~%" real fake)))

(defun optimize-topology (ast)
   (let* ((edge-set (find-edge-set ast (get-routes ast)))
          (node-set (create-hash-set (defined-nodes ast)))
          (mapping (bfs-ordering edge-set node-set)))
      (print-mapping mapping)
      (setf (defined-nodes ast) mapping)
      (do-clauses (clauses ast) (:head head :body body)
         (flip-nodes mapping (append head body)))
      ast))