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
   ((num-searches
     :initarg :num-searches
     :accessor snap-search-num)
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
   (make-instance 'snap-search-data :nodes ids :edges edge-table
                                    :rand-state (sb-ext:seed-random-state 0)
                                    :fraction fraction
                                    :num-searches num-searches)))

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

(defmethod data-input-node-axioms ((obj snap-search-data) (n integer))
   (let ((facts (snap-search-build-edges obj n))
         (rnd (random 100 (snap-rand-state obj)))
         (n-nodes (hash-table-count (snap-nodes obj))))
    (push (make-subgoal "value" (list (make-int n :type-int))) facts)
    (when (< rnd (snap-search-fraction obj))
      (let (targets
            (ls (make-nil))
            (n (truncate (* n-nodes (/ (coerce (snap-search-fraction obj) 'float) 100)))))
       (loop while (< (length targets) n)
             do (let ((id (random n-nodes (snap-rand-state obj))))
                  (unless (member id targets)
                   (setf ls (make-cons (make-int id :type-int) ls))
                   (push id targets))))
       (push (make-subgoal "search" (list (make-int (snap-search-id obj) :type-int) ls)) facts)
       (incf (snap-search-id obj))))
    facts))
