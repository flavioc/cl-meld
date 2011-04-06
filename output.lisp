
(in-package :cl-meld)

(define-condition output-invalid-error (error)
   ((text :initarg :text :reader text)))

(defmacro with-memory-stream (s &body body)
   `(let ((,s (make-in-memory-output-stream)))
      ,@body
      s))

(defun output-external-functions (code s)
   (format s "#include \"extern_functions.h\"~%Register (*extern_functs[])() = {")
   (do-externs code (:name name :id id)
      (format s "~a(Register (*)())&~a" (if (> id 0) ", " "") name))
   (format s "};~%")
   (format s "int extern_functs_args[] = {")
   (do-externs code (:types typs)
      (format s "~a," (length typs)))
   (format s "};~%"))
   
(defun output-int (int)
   (loop for i upto 3
      collect (ldb (byte 8 (* i 8)) int)))
(defun output-float (flt) (output-int (encode-float32 (coerce flt 'float))))

(defun output-value (val)
   (cond
      ((vm-int-p val) (list #b000001 (output-int (vm-int-val val))))
      ((vm-float-p val) (list #b000000 (output-float (vm-float-val val))))
      ((vm-addr-p val) (list  #b000101 (output-int (vm-addr-num val))))
      ((vm-host-id-p val) (list #b000011))
      ((vm-nil-p val) (list #b000100))
      ((tuple-p val) (list #b011111))
      ((reg-p val) (list (logior #b100000 (logand #b011111 (reg-num val)))))
      ((reg-dot-p val) (list #b000010 (list (reg-dot-field val) (reg-num (reg-dot-reg val)))))
      (t (error 'output-invalid-error :text "invalid expression value"))))

(defmacro add-byte (b vec) `(vector-push-extend ,b ,vec))
(defun add-bytes (vec &rest bs)
   (dolist (b bs) (add-byte b vec)))

(defmacro do-vm-values (vec vals &rest instrs)
   (labels ((map-value (i) (case i (1 'first-value) (2 'second-value) (3 'third-value)))
            (map-value-bytes (i) (case i (1 'first-value-bytes) (2 'second-value-bytes) (3 'third-value-bytes))))
      (let* ((i 0)
             (vals-code (mapcar #'(lambda (val) `(output-value ,val)) vals))
             (instrs-code `(progn ,@(mapcar #'(lambda (instr) `(add-byte ,instr ,vec)) instrs)
                              ,@(loop for i from 1 upto (length vals)
                                    collect `(dolist (bt ,(map-value-bytes i))
                                                (add-byte bt ,vec))))))
         (reduce #'(lambda (all val)
                     (incf i)
                     `(let* ((value ,val)
                           (,(map-value i) (first value))
                           (,(map-value-bytes i) (second value)))
                              ,all))
                  vals-code :initial-value instrs-code :from-end nil))))

(defun get-op-byte (op)
   (case op
      (:float-not-equal #b00000)
      (:int-not-equal #b00001)
      (:float-equal #b00010)
      (:int-equal #b00011)
      (:float-lesser #b00100)
      (:int-lesser #b00101)
      (:float-lesser-equal #b00110)
      (:int-lesser-equal #b00111)
      (:float-greater #b01000)
      (:int-greater #b01001)
      (:float-greater-equal #b01010)
      (:int-greater-equal #b01011)
      (:float-mod #b01100)
      (:int-mod #b01101)
      (:float-plus #b01110)
      (:int-plus #b01111)
      (:float-minus #b10000)
      (:int-minus #b10001)
      (:float-mul #b10010)
      (:int-mul #b10011)
      (:float-div #b10100)
      (:int-div #b10101)
      (:addr-equal #b10111)
      (:addr-not-equal #b10110)
      (otherwise (error 'output-invalid-error :text "Unknown operation to convert"))))
      
(defun reg-to-byte (reg) (reg-num reg))

(defun lookup-tuple-id (ast tuple)
   (do-definitions ast (:id id :name name)
      (if (equal name tuple) (return-from lookup-tuple-id id))))
      
(defun lookup-extern-id (ast extern)
   (do-externs ast (:id id :name name)
      (if (equal name extern) (return-from lookup-extern-id id))))
      
(defun output-match (match vec fs)
   (let* ((val (output-value (match-right match)))
          (val-byte (first val))
          (val-bytes (second val))
          (reg-dot (match-left match))
          (field (reg-dot-field reg-dot)))
      (add-byte field vec)
      (add-byte (logior fs val-byte) vec)
      (dolist (by val-bytes)
         (add-byte by vec))))
      
(defun output-matches (matches vec)
   (cond
      ((null matches)
         (add-byte #b00000000 vec)
         (add-byte #b11000000 vec))
      ((one-elem-p matches) (output-match (first matches) vec #b01000000))
      (t (output-match (first matches) vec #b00000000)
         (output-matches (rest matches) vec))))

(defun write-int-vec (vec int &optional (pos 0))
   (let ((ls (output-int int)))
      (setf (aref vec pos) (first ls)
            (aref vec (1+ pos)) (second ls))))
            
(defmacro jumps-here (vec)
   `(progn
      (add-byte #b0 ,vec)
      (add-byte #b0 ,vec)))
      
(defmacro write-jump (vec jump-many &body body)
   (with-gensyms (pos)
      `(let ((,pos (length ,vec)))
          ,@body
         (write-int-vec ,vec (- (length ,vec) ,pos) (+ ,pos ,jump-many)))))
         
(defun output-cons-type (typ)
   (case typ
      (:type-list-int 0)
      (:type-list-float 1)
      (:type-list-addr 2)))
                  
(defparameter *value-mask* #b00111111)
(defparameter *reg-mask* #b00011111)
(defparameter *op-mask* #b00011111)
(defparameter *tuple-id-mask* #b01111111)
(defparameter *extern-id-mask* #b01111111)

(defun output-instr (ast instr vec)
   (case (instr-type instr)
      (:return (add-byte #x0 vec))
      (:op (let ((op (get-op-byte (vm-op-op instr))))
               (do-vm-values vec ((vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr))
                           #b11000000
                           (logand *value-mask* first-value)
                           (logand *value-mask* second-value)
                           (logand *value-mask* third-value)
                           (logand *op-mask* op))))
      (:alloc (let ((tuple-id (lookup-tuple-id ast (vm-alloc-tuple instr)))
                    (reg (reg-to-byte (vm-alloc-reg instr))))
                  (add-byte #b01000000 vec)
                  (add-byte (logand *tuple-id-mask* tuple-id) vec)
                  (add-byte (logand *reg-mask* reg) vec)))
      (:send (add-byte #b00001000 vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-from instr))) vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-to instr))) vec))
      (:call (let ((extern-id (lookup-extern-id ast (vm-call-name instr)))
                   (args (vm-call-args instr)))
               (add-byte #b00100000 vec)
               (add-byte (logand *extern-id-mask* extern-id) vec)
               (add-byte (logand *reg-mask* (reg-to-byte (vm-call-dest instr))) vec)
               (dolist (arg args)
                  (let ((res (output-value arg)))
                     (add-byte (first res) vec)
                     (dolist (b (second res)) (add-byte b vec))))))
      (:if (let ((reg-b (reg-to-byte (if-reg instr))))
             (write-jump vec 2
               (add-byte #b01100000 vec)
               (add-byte (logand *reg-mask* reg-b) vec)
               (jumps-here vec)
               (output-instrs (if-instrs instr) vec ast))))
      (:iterate (write-jump vec 2
                  (add-byte #b10100000 vec)
                  (add-byte (lookup-tuple-id ast (iterate-name instr)) vec)
                  (jumps-here vec)
                  (output-matches (iterate-matches instr) vec)
                  (output-instrs (iterate-instrs instr) vec ast)
                  (add-byte #b00000001 vec)))
      (:move (do-vm-values vec ((move-from instr) (move-to instr))
                #b00110000
                (logand *value-mask* first-value)
                (logand *value-mask* second-value)))
      (:move-nil (do-vm-values vec ((move-nil-to instr))
                  #b01110000
                  (logand *value-mask* first-value)))
      (:test-nil (do-vm-values vec ((vm-test-nil-place instr) (vm-test-nil-dest instr))
                     #b00000011
                     (logand *value-mask* first-value)
                     (logand *value-mask* second-value)))
      (:not (do-vm-values vec ((vm-not-place instr) (vm-not-dest instr))
               #b00000111
               (logand *value-mask* first-value)
               (logand *value-mask* second-value)))
      (:cons (do-vm-values vec ((vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr))
                  #b00000100
                  (output-cons-type (vm-cons-type instr))
                  (logand *value-mask* first-value)
                  (logand *value-mask* second-value)
                  (logand *value-mask* third-value)))
      (:head (do-vm-values vec ((vm-head-cons instr) (vm-head-dest instr))
                  #b00000101
                  (output-cons-type (vm-head-type instr))
                  (logand *value-mask* first-value)
                  (logand *value-mask* second-value)))
      (:tail (do-vm-values vec ((vm-tail-cons instr) (vm-tail-dest instr))
                  #b000000110
                  (output-cons-type (vm-tail-type instr))
                  (logand *value-mask* first-value)
                  (logand *value-mask* second-value)))
      (otherwise (error 'output-invalid-error :text "unknown instruction to output"))))
                
(defun output-instrs (ls vec ast)
   (dolist (instr ls)
      (output-instr ast instr vec)))
                             
(defun output-processes (ast code)
   (do-processes code (:instrs instrs :operation collect)
      (letret (vec (create-bin-array))
         (output-instrs instrs vec ast))))

(defun output-arg-type (typ vec)
   (add-byte
      (case typ
         (:type-int #b0000)
         (:type-float #b0001)
         (:type-addr #b0010)
         (:type-list-int #b0011)
         (:type-list-float #b0100)
         (:type-list-addr #b0101)
         (otherwise (error 'output-invalid-error :text "invalid arg type")))
      vec))
   
(defun output-aggregate-type (agg typ)
   (case agg
      (:first #b0001)
      (:min (case typ
               (:type-int #b0011)
               (:type-float #b0110)))
      (:max (case typ
               (:type-int #b0010)
               (:type-float #b0101)))
      (:sum (case typ
               (:type-int #b0100)
               (:type-float #b0111)))))
               
(defun output-aggregate (types)
   (let ((agg (find-if #'aggregate-p types)))
      (if agg
         (let ((pos (position-if #'aggregate-p types))
               (agg (aggregate-agg agg))
               (typ (aggregate-type agg)))
            (logior (logand #b11110000 (ash (output-aggregate-type agg typ) 4))
                    (logand #b00001111 pos)))
         #b00000000)))
         
(defun output-properties (types)
   (let ((agg (find-if #'aggregate-p types)))
      (if agg
         #b00000001
         #b00000000)))

(defparameter *max-tuple-name* 32)
(defparameter *max-tuple-args* 32)

(defun output-tuple-name (name vec)
   (let ((len (length name)))
      (loop for x being the elements of name
         do (add-byte (char-code x) vec))
      (loop for i from (1+ len) to *max-tuple-name*
         do (add-byte #b0 vec))))
         
(defun output-tuple-type-args (types vec)
   (let* ((args (definition-arg-types types))
          (len (length args)))
      (dolist (typ args)
         (output-arg-type typ vec))
      (loop for i from (1+ len) to *max-tuple-args*
         do (add-byte #b0 vec))))
      
(defun output-descriptors (ast)
   (do-definitions ast (:name name :types types :operation collect)
      (letret (vec (create-bin-array))
         (add-byte (output-properties types) vec) ; property byte
         (add-byte (output-aggregate types) vec) ; aggregate byte
         (add-byte (length types) vec) ; number of args
         (output-tuple-type-args types vec) ; argument type information
         (output-tuple-name name vec)))) ;; predicate name
            
(defparameter *total-written* 0)

(defun write-hexa (stream int) (incf *total-written*) (write-byte int stream))
(declaim (inline write-hexa))

(defun write-vec (stream vec)
   (loop for b being the elements of vec
      do (write-hexa stream b)))
      
(defun write-short-stream (stream int)
   (let ((ls (output-int int)))
      (write-hexa stream (first ls))
      (write-hexa stream (second ls))))

(defun write-int-stream (stream int)
   (dolist (part (output-int int))
      (write-hexa stream part)))

(defun write-nodes (stream nodes)
   (write-int-stream stream (length nodes))
   (dolist (node nodes) (write-int-stream stream node)))

(defun do-output-code (ast code stream)
   (write-hexa stream (length (definitions ast)))
   (write-nodes stream (defined-nodes ast))
   (let ((processes (output-processes ast code))
         (descriptors (output-descriptors ast)))
      (loop for vec-desc in descriptors
            for vec-proc in processes
            do (write-short-stream stream (length vec-proc)) ; write code size first
            do (write-vec stream vec-desc))
      (dolist (vec processes) (write-vec stream vec))))
   
(defmacro with-output-file ((stream file) &body body)
   `(with-open-file (,stream ,file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      ,@body))
   
(defun output-code (ast code file)
   (let ((*total-written* 0)
         (byte-file (concatenate 'string file ".m"))
         (ast-file (concatenate 'string file ".m.ast"))
         (code-file (concatenate 'string file ".m.code")))
      (with-open-file (stream byte-file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :element-type '(unsigned-byte 8))
         (do-output-code ast code stream))
      (with-output-file (stream ast-file)
         (format stream "~a~%" ast))
      (with-output-file (stream code-file)
         (format stream "~a" (print-vm code)))))
