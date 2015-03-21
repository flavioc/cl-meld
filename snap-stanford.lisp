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
     :accessor snap-nodes)))

(defun snap-file-read (filename)
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
      (make-instance 'snap-data :nodes ids :edges edge-table)))

(defun snap-file-nodes (snap)
 (let ((nodes (make-mapping-set)))
   (loop for key being the hash-keys of (snap-nodes snap)
         using (hash-value node)
           do (add-mapping nodes node key))
   nodes))

(defmethod data-input-node-axioms ((obj snap-data) (n integer))
   (multiple-value-bind (vec found-p) (gethash n (snap-edges obj))
    (unless found-p
     (return-from data-input-node-axioms nil))
    (loop for other-node across vec
          collect (make-subgoal "edge" (list (make-addr other-node) (make-float 10))))))
