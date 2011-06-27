
(in-package :cl-meld)
         
(defun matches-op-host-p (instr)
   (and (vm-op-p instr)
        (reg-p (vm-op-dest instr))
        (reg-eq-p (vm-op-dest instr) (make-reg 0))
        (vm-host-id-p (vm-op-v1 instr))
        (vm-addr-p (vm-op-v2 instr))
        (op-eq-p (vm-op-op instr) :addr-equal)))
        
(defun matches-op-if-0-p (instr)
   (and (if-p instr)
        (reg-eq-p (if-reg instr) (make-reg 0))))
        
(defun add-instrs-to-node (hash node instrs)
   (multiple-value-bind (other-instrs found-p) (gethash node hash)
      (declare (ignore found-p))
      (setf (gethash node hash) (append other-instrs instrs))))

(defun get-target-node (op-instr) (addr-num (vm-op-v2 op-instr)))

(defun select-node-init (start-instrs)
   (let ((hash (make-hash-table))
         (new-start nil)
         (ptr nil)
         (current start-instrs))
      (loop while (not (null current))
            do (cond
                  ((at-least-n-p current 2)
                     (cond
                        ((and (matches-op-host-p (nth 0 current))
                              (matches-op-if-0-p (nth 1 current)))
                           (add-instrs-to-node hash (get-target-node (nth 0 current)) (if-instrs (nth 1 current)))
                           (setf current (drop-first-n current 2)))
                        (new-start
                           (setf (cdr ptr) current
                                 ptr current
                                 current (cdr current)
                                 (cdr ptr) nil))
                        (t
                           (setf new-start current
                                 ptr current
                                 current (cdr current)
                                 (cdr ptr) nil))))
                  (t
                     (cond
                        (new-start
                           (setf (cdr ptr) current
                                 current nil))
                        (t
                           (setf new-start current
                                 current nil))))))
      (values hash new-start)))
            
(defun make-vm-select-with-rules (hash)
   (letret (instr (make-vm-select-node))
      (iterate-hash (hash node instrs)
         (vm-select-node-push instr node instrs))))
      
(defun optimize-init (ast code)
   (let ((def (find-init-predicate (definitions ast))))
      (assert (not (null def)))
      (with-definition def (:name init-name)
         (with-process (vm-find code init-name) (:instrs instrs :proc proc)
            (multiple-value-bind (hash to-keep) (select-node-init instrs)
               (let ((new-instr (make-vm-select-with-rules hash)))
                  (setf (process-instrs proc) (cons new-instr to-keep))))))))
               
(defun optimize-code (ast code)
   (optimize-init ast code)
   code)
   
