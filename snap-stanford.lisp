(in-package :cl-meld)

(defparameter *snap-id* 0)

(defun map-snap-node (id ids)
   (multiple-value-bind (val found-p) (gethash id ids)
      (if found-p
       val
       (progn
         (setf val *snap-id*)
         (incf *snap-id*)
         (setf (gethash id ids) val)
         val))))

(defclass snap-data ()
   ((edges
     :initarg :edges
     :accessor snap-edges)
    (nodes
     :initarg :nodes
     :accessor snap-nodes)
    (rnd
     :initarg :rand-state
     :accessor snap-rand-state)))

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

(defun read-snap-file (filename)
   (let ((*snap-id* 0)
         (edge-table (make-hash-table))
         (ids (make-hash-table)))
      (cl-csv:do-csv (row (pathname filename) :separator #\Tab :quote #\#)
         (let ((node1 (map-snap-node (parse-integer (first row)) ids))
               (node2 (map-snap-node (parse-integer (second row)) ids)))
          (multiple-value-bind (edges found-p) (gethash node1 edge-table)
           (cond
            (found-p
             (vector-push-extend node2 edges))
            (t
             (setf edges (make-array 16 :fill-pointer 0 :adjustable t))
             (vector-push node2 edges)
             (setf (gethash node1 edge-table) edges))))))
      (values ids edge-table)))

(defun snap-file-read (filename)
 (multiple-value-bind (ids edge-table) (read-snap-file filename)
      (make-instance 'snap-data :nodes ids :edges edge-table
                                 :rand-state (sb-ext:seed-random-state 0))))

(defun snap-search-file-read (filename num-searches fraction)
 (multiple-value-bind (ids edge-table) (read-snap-file filename)
  (let ((source (make-hash-table))
        (rnd (sb-ext:seed-random-state 0)))
      (loop while (not (= (hash-table-count source) num-searches))
            do (let ((i (random (hash-table-count ids) rnd)))
                  (setf (gethash i source) t)))
      (make-instance 'snap-search-data :nodes ids :edges edge-table
                                       :rand-state rnd
                                       :fraction fraction
                                       :nodes-search source))))

(defmethod data-input-nodes ((snap snap-data))
 (let ((nodes (make-mapping-set)))
   (loop for key being the hash-keys of (snap-nodes snap)
         using (hash-value node)
           do (add-mapping nodes node node))
   nodes))

(defmethod data-input-node-axioms ((obj snap-data) (n integer))
   (multiple-value-bind (vec found-p) (gethash n (snap-edges obj))
    (unless found-p
     (return-from data-input-node-axioms nil))
    (loop for other-node across vec
          collect (make-subgoal "edge" (list (make-addr other-node)
                (make-float (1+ (random 500 (snap-rand-state obj)))))))))

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
   (let ((facts (snap-search-build-edges obj n))
         (rnd (random 100 (snap-rand-state obj)))
         (n-nodes (hash-table-count (snap-nodes obj))))
    (push (make-subgoal "value" (list (make-int n :type-int))) facts)
    (when (gethash n (snap-search-source-nodes obj))
      (let (targets
            (ls (make-nil))
            (n-targets (truncate (* n-nodes (/ (coerce (snap-search-fraction obj) 'float) 100)))))
       (when (= (mod n 5) 0)
         (setf targets (snap-search-find-path n (snap-edges obj) n-targets (snap-rand-state obj))))
       (unless targets
          (loop while (< (length targets) n-targets)
                do (let ((id (random n-nodes (snap-rand-state obj))))
                     (unless (member id targets)
                      (push id targets)))))
       (loop for id in targets
             do (setf ls (make-cons (make-int id :type-int) ls)))
       (push (make-subgoal "search" (list (make-int (snap-search-id obj) :type-int) ls)) facts)
       (incf (snap-search-id obj))))
    facts))
