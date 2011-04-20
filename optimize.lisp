
(in-package :cl-meld)

(defun find-init-predicate (ast)
   (do-definitions ast (:definition def)
      (when (definition-has-option-p def :init-tuple)
         (return-from find-init-predicate def))))
         
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
   (let ((hash (make-hash-table)))
      (labels ((aux (instrs)
               (cond
                  ((at-least-n-p instrs 2)
                     (if (and (matches-op-host-p (nth 0 instrs))
                              (matches-op-if-0-p (nth 1 instrs)))
                        (let ((not-selected (aux (drop-first-n instrs 2))))
                           (add-instrs-to-node hash (get-target-node (nth 0 instrs)) (if-instrs (nth 1 instrs)))
                           not-selected)
                        (let ((not-selected (aux (drop-first-n instrs 1))))
                           (get-first-n instrs 1 not-selected))))
                  (t instrs))))
         (values hash (aux start-instrs)))))
            
(defun make-vm-select-with-rules (hash)
   (letret (instr (make-vm-select-node))
      (iterate-hash (hash node instrs)
         (vm-select-node-push instr node instrs))))
      
(defun optimize-init (ast code)
   (let ((def (find-init-predicate ast)))
      (with-definition def (:name init-name)
         (with-process (vm-find code init-name) (:instrs instrs :proc proc)
            (multiple-value-bind (hash to-keep) (select-node-init instrs)
               (let ((new-instr (make-vm-select-with-rules hash)))
                  (setf (process-instrs proc) (cons new-instr to-keep))))))))
               
(defun optimize-code (ast code)
   (optimize-init ast code)
   )
   