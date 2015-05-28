(in-package :cl-meld)

(defclass snap-data ()
   ((edges
     :initarg :edges
     :accessor snap-edges)
    (nodes
     :initarg :nodes
     :accessor snap-nodes)))

(defclass snap-basic-data (snap-data)
   ())

(defclass snap-pagerank-data (snap-data)
   ((reverse-edges
     :initarg :reverse-edges
     :accessor snap-reverse-edges)
    (reverse-weights
     :initarg :reverse-weights
     :accessor snap-reverse-weights)))

(defclass snap-search-data (snap-data)
   ((nodes-search
     :initarg :nodes-search
     :accessor snap-search-source-nodes)
    (fraction
     :initarg :fraction
     :accessor snap-search-fraction)
    (id
     :initform 0
     :accessor snap-search-id)))

(defun snap-extend-vec (table vec found-p key data &key (force-p nil))
  (cond
    (found-p
     (when (or force-p (not (position data vec)))
      (vector-push-extend data vec)))
    (t
      (setf vec (make-array 16 :fill-pointer 0 :adjustable t))
      (vector-push data vec)
      (setf (gethash key table) vec))))

(defun read-snap-file (filename)
   (let ((edge-table (make-hash-table))
         (ids (make-hash-table)))
      (cl-csv:do-csv (row (pathname filename) :separator #\Tab :quote #\#)
         (let ((node1 (parse-integer (first row)))
               (node2 (parse-integer (second row))))
          (setf (gethash node1 ids) node1)
          (setf (gethash node2 ids) node2)
          (multiple-value-bind (edges found-p) (gethash node1 edge-table)
           (snap-extend-vec edge-table edges found-p node1 node2))))
      (values ids edge-table)))

(defun snap-file-read (filename)
 (multiple-value-bind (ids edge-table) (read-snap-file filename)
   (make-instance 'snap-data :nodes ids :edges edge-table)))

(defun snap-basic-file-read (filename)
   (multiple-value-bind (ids edge-table) (read-snap-file filename)
    (make-instance 'snap-basic-data :nodes ids :edges edge-table)))

(defun snap-undirected-file-read (filename)
   (multiple-value-bind (ids edge-table) (read-snap-file filename)
      (loop for n being the hash-keys of ids
            do (multiple-value-bind (vec found-p) (gethash n edge-table)
               (when found-p
                 (loop for other-node across vec
                       do (multiple-value-bind (vec2 found2-p) (gethash other-node edge-table)
                            (snap-extend-vec edge-table vec2 found2-p other-node n))))))
      (make-instance 'snap-basic-data :nodes ids :edges edge-table)))

(defun snap-pagerank-file-read (filename)
   (multiple-value-bind (ids edge-table) (read-snap-file filename)
      (let ((rev-edges (make-hash-table))
            (rev-weights (make-hash-table)))
      (loop for n being the hash-keys of ids
            do (multiple-value-bind (vec found-p) (gethash n edge-table)
               (when found-p
                (let* ((len (length vec))
                       (frac (/ 1.0 len)))
                    (loop for other-node across vec
                         do (multiple-value-bind (rev-vec found2-p) (gethash other-node rev-edges)
                              (snap-extend-vec rev-edges rev-vec found2-p other-node n))
                         do (multiple-value-bind (rweight-vec found2-p) (gethash other-node rev-weights)
                              (snap-extend-vec rev-weights rweight-vec found2-p other-node frac :force-p t)))))))
      (make-instance 'snap-pagerank-data :nodes ids :edges edge-table
                                         :reverse-edges rev-edges
                                         :reverse-weights rev-weights))))

(defun snap-search-file-read (filename num-searches fraction)
 (multiple-value-bind (ids edge-table) (read-snap-file filename)
  (let ((source (make-hash-table))
        (rnd (sb-ext:seed-random-state 0)))
      (loop while (not (= (hash-table-count source) num-searches))
            do (let ((i (random (hash-table-count ids) rnd)))
                  (setf (gethash i source) t)))
      (make-instance 'snap-search-data :nodes ids :edges edge-table
                                       :fraction fraction
                                       :nodes-search source))))

(defmethod data-input-nodes ((snap snap-data))
 (let ((nodes (make-mapping-set)))
   (loop for key being the hash-keys of (snap-nodes snap)
         using (hash-value node)
           do (add-mapping nodes node node))
   nodes))

(defun compute-snap-edge-weight (rnd) (1+ (random 500 rnd)))

(defmethod data-input-node-axioms ((obj snap-data) (n integer))
   (multiple-value-bind (vec found-p) (gethash n (snap-edges obj))
    (unless found-p
     (return-from data-input-node-axioms nil))
    (let ((rnd (sb-ext:seed-random-state 0)))
       (loop for other-node across vec
             collect (make-subgoal "edge" (list (make-addr other-node)
                         (let ((weight (compute-snap-edge-weight rnd)))
                          (make-float weight))))))))

(defmethod data-input-node-axioms ((obj snap-basic-data) (n integer))
   (multiple-value-bind (vec found-p) (gethash n (snap-edges obj))
    (unless found-p
     (return-from data-input-node-axioms nil))
    (loop for other-node across vec
          collect (make-subgoal "edge" (list (make-addr other-node))))))

(defmethod data-input-node-axioms ((obj snap-pagerank-data) (n integer))
  (let (axioms)
   (multiple-value-bind (vec found-p) (gethash n (snap-edges obj))
    (when found-p
       (let* ((len (length vec))
              (frac (/ 1.0 len)))
          (loop for other-node across vec
                do (push (make-subgoal "output" (list (make-addr other-node) (make-float frac)))
                         axioms)))))
   (multiple-value-bind (vec found-p) (gethash n (snap-reverse-edges obj))
    (when found-p
     (multiple-value-bind (weights donotcare) (gethash n (snap-reverse-weights obj))
      (assert donotcare)
      (loop for other-node across vec
            for weight across weights
            do (push (make-subgoal "input" (list (make-addr other-node) (make-float weight)))
                     axioms)))))
   axioms))

(defmethod data-input-dump ((obj snap-data) filename)
 (with-open-file (stream filename :direction :output)
   (let ((rnd (sb-ext:seed-random-state 0)))
      (loop for n being the hash-keys of (snap-nodes obj)
            do (multiple-value-bind (vec found-p) (gethash n (snap-edges obj))
                  (when vec
                   (loop for other-node across vec
                        do (let ((weight (compute-snap-edge-weight rnd)))
                            (format stream "~a ~a ~a~%" n other-node weight)))))))))

(defun snap-stanford-dump (filename outfile)
 (let ((snap (snap-file-read filename)))
   (data-input-dump snap outfile)))

(defun snap-search-build-edges (obj n)
   (multiple-value-bind (vec found-p) (gethash n (snap-edges obj))
    (unless found-p
     (return-from snap-search-build-edges nil))
    (loop for other-node across vec
          collect (make-subgoal "edge" (list (make-addr other-node))))))

(defun snap-search-find-path (n edges n-targets rnd)
 (let ((reached (make-hash-table)))
  (labels ((aux (x)
               (setf (gethash x reached) t)
               (when (> (hash-table-count reached) (* 2 n-targets))
                (return-from aux nil))
               (multiple-value-bind (ls found-p) (gethash x edges)
                  (when found-p
                   (loop for ed being the elements of ls
                         do (multiple-value-bind (ig found-p) (gethash ed reached)
                              (unless found-p
                                 (unless (aux ed)
                                  (return-from aux nil)))))))
               t))
   (aux n)
   (get-first-n (shuffle-list (loop for key being the hash-keys of reached collect key) rnd)
                  n-targets))))

(defmethod data-input-node-axioms ((obj snap-search-data) (n integer))
   (let* ((facts (snap-search-build-edges obj n))
          (rnd (sb-ext:seed-random-state 0))
          (n-nodes (hash-table-count (snap-nodes obj))))
    (push (make-subgoal "value" (list (make-int n :type-int))) facts)
    (when (gethash n (snap-search-source-nodes obj))
      (let (targets
            (ls (make-nil))
            (n-targets (truncate (* n-nodes (/ (coerce (snap-search-fraction obj) 'float) 100)))))
       (when (= (mod n 5) 0)
         (setf targets (snap-search-find-path n (snap-edges obj) n-targets rnd)))
       (unless targets
          (loop while (< (length targets) n-targets)
                do (let ((id (random n-nodes rnd)))
                     (unless (member id targets)
                      (push id targets)))))
       (loop for id in targets
             do (setf ls (make-cons (make-int id :type-int) ls)))
       (push (make-subgoal "search" (list (make-int (snap-search-id obj) :type-int) ls)) facts)
       (incf (snap-search-id obj))))
    facts))
