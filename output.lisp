
(in-package :cl-meld)

(define-condition output-invalid-error (error)
   ((text :initarg :text :reader text)))

(defparameter *output-string-constants* nil)
(defun push-string-constant (str)
	"Adds string constant to database if not found and returns the string integer code."
	(let ((pos (position str *output-string-constants* :test #'string-equal)))
		(if pos
			pos
			(progn
				(push-end str *output-string-constants*)
				(1- (length *output-string-constants*))))))

(defmacro with-memory-stream (s &body body)
   `(let ((,s (make-in-memory-output-stream)))
      ,@body
      s))
   
(defun output-int (int)
   (loop for i upto 3
      collect (ldb (byte 8 (* i 8)) int)))
(defun output-float (flt) (output-int (encode-float32 (coerce flt 'float))))
(defun output-string (str)
	(map 'list #'char-code str))

(defun output-value (val)
   (cond
      ((vm-int-p val) (list #b000001 (output-int (vm-int-val val))))
      ((vm-float-p val) (list #b000000 (output-float (vm-float-val val))))
		((vm-string-constant-p val)
			(let* ((str (vm-string-constant-val val))
					 (code (push-string-constant str)))
				(list #b000110 (output-int code))))
      ((vm-addr-p val) (list  #b000101 (output-int (vm-addr-num val))))
      ((vm-host-id-p val) (list #b000011))
      ((vm-nil-p val) (list #b000100))
      ((vm-world-p val) (output-value (make-vm-int (number-of-nodes *nodes*))))
      ((tuple-p val) (list #b011111))
      ((reg-p val) (list (logior #b100000 (logand #b011111 (reg-num val)))))
      ((reg-dot-p val) (list #b000010 (list (reg-dot-field val) (reg-num (reg-dot-reg val)))))
		((vm-argument-p val) (list #b00000111 (list (vm-argument-id val))))
		((vm-constant-p val) (list #b00001000 (output-int (lookup-const-id (vm-constant-name val)))))
      (t (error 'output-invalid-error :text (tostring "Invalid expression value: ~a" val)))))

(defmacro add-byte (b vec) `(vector-push-extend ,b ,vec))
(defun add-bytes (vec ls)
	(dolist (b ls) (add-byte b vec)))

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
      (:addr-not-equal #b10110)
		(:addr-equal #b10111)
		(:addr-greater #b11000)
		(:bool-or #b11001)
      (otherwise (error 'output-invalid-error :text (tostring "Unknown operation to convert ~a" op)))))
      
(defun reg-to-byte (reg) (reg-num reg))
      
(defun lookup-extern-id (ast extern)
   (do-externs ast (:id id :name name)
      (if (string-equal name extern) (return-from lookup-extern-id id))))

(defun lookup-const-id (const)
	(do-constant-list *consts* (:name name :id id)
		(if (string-equal name const) (return-from lookup-const-id id))))
      
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

(defconstant +code-offset-size+ 4)
(defun write-offset (vec off &optional (pos 0))
   (let ((ls (output-int off)))
      (loop for i from 0 to 3
            for part in ls
            do (setf (aref vec (+ pos i)) part))))
            
(defmacro jumps-here (vec)
   `(progn
      (add-byte #b0 ,vec)
      (add-byte #b0 ,vec)
      (add-byte #b0 ,vec)
      (add-byte #b0 ,vec)))
      
(defmacro save-pos ((pos vec) &body body)
   `(let ((,pos (length ,vec)))
      ,@body))
      
(defmacro write-jump (vec jump-many &body body)
   (with-gensyms (pos)
      `(save-pos (,pos ,vec)
          ,@body
         (write-offset ,vec (- (length ,vec) ,pos) (+ ,pos ,jump-many)))))
         
(defun output-cons-type (typ)
	(assert (not (null typ)))
   (case typ
      (:type-list-int 0)
      (:type-list-float 1)
      (:type-list-addr 2)))

(defparameter *value-mask* #b00111111)
(defparameter *reg-mask* #b00011111)
(defparameter *op-mask* #b00011111)
(defparameter *tuple-id-mask* #b01111111)
(defparameter *extern-id-mask* #b01111111)

(defun iterate-options-bytes (iter)
	(let ((opt #b00000000)
			(snd #b00000000))
		(when (iterate-random-p iter)
			(setf opt (logior opt #b00000001)))
		(when (iterate-to-delete-p iter)
			(setf opt (logior opt #b00000010)))
		(when (iterate-min-p iter)
			(setf snd (iterate-min-arg iter))
			(setf opt (logior opt #b00000100)))
		(values opt snd)))

(defun output-instr (instr vec)
   (case (instr-type instr)
      (:return (add-byte #x0 vec))
      (:next (add-byte #x1 vec))
      (:return-linear (add-byte #b11010000 vec))
      (:return-derived (add-byte #b11110000 vec))
		(:save-original
			(write-jump vec 1
				(add-byte #b00010010 vec)
				(jumps-here vec)
				(output-instrs (vm-save-original-code instr) vec)))
      (:reset-linear
         (write-jump vec 1
            (add-byte #b00001110 vec)
            (jumps-here vec)
            (output-instrs (vm-reset-linear-instrs instr) vec)))
		(:end-linear
			(add-byte #b00001111 vec))
      (:remove
            (let ((reg (vm-remove-reg instr)))
               (add-byte #b10000000 vec)
               (add-byte (logand *reg-mask* (reg-to-byte reg)) vec)))
      (:op (let ((op (get-op-byte (vm-op-op instr))))
               (do-vm-values vec ((vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr))
                           #b11000000
                           (logand *value-mask* first-value)
                           (logand *value-mask* second-value)
                           (logand *value-mask* third-value)
                           (logand *op-mask* op))))
      (:alloc (let ((tuple-id (lookup-def-id (vm-alloc-tuple instr)))
                    (reg (reg-to-byte (vm-alloc-reg instr))))
                  (add-byte #b01000000 vec)
                  (add-byte (logand *tuple-id-mask* tuple-id) vec)
                  (add-byte (logand *reg-mask* reg) vec)))
      (:send (add-byte #b00001000 vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-from instr))) vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-to instr))) vec))
      (:call (let ((extern-id (lookup-external-function-id (vm-call-name instr)))
                   (args (vm-call-args instr)))
               (add-byte #b00100000 vec)
               (add-byte (logand *extern-id-mask* extern-id) vec)
               (add-byte (length args) vec)
               (add-byte (logand *reg-mask* (reg-to-byte (vm-call-dest instr))) vec)
               (dolist (arg args)
                  (let ((res (output-value arg)))
                     (add-byte (first res) vec)
                     (add-bytes vec (second res))))))
      (:if (let ((reg-b (reg-to-byte (vm-if-reg instr))))
             (write-jump vec 2
               (add-byte #b01100000 vec)
               (add-byte (logand *reg-mask* reg-b) vec)
               (jumps-here vec)
               (output-instrs (vm-if-instrs instr) vec))))
      (:iterate (write-jump vec 4
                  (add-byte #b10100000 vec)
                  (add-byte (lookup-def-id (iterate-name instr)) vec)
						(multiple-value-bind (b1 b2) (iterate-options-bytes instr)
							(add-byte b1 vec)
							(add-byte b2 vec))
                  (jumps-here vec)
                  (output-matches (iterate-matches instr) vec)
                  (output-instrs (iterate-instrs instr) vec)
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
      (:convert-float (do-vm-values vec ((vm-convert-float-place instr) (vm-convert-float-dest instr))
                        #b00001001
                        (logand *value-mask* first-value)
                        (logand *value-mask* second-value)))
      (:select-node (let* ((total-nodes (number-of-nodes *nodes*))
                           (size-header (* 2 +code-offset-size+))
                           (hash (make-hash-table))
                           (end-hash (make-hash-table)))
                        (add-byte #b00001010 vec)
                        (save-pos (start vec)
                           (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) ; size of complete select instruction
                           (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) ; table size
                           (write-offset vec total-nodes (+ start +code-offset-size+))
                           (loop for i from 0 to (1- total-nodes)
                                 do (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec))
                           (save-pos (begin-instrs vec)
                              (vm-select-node-iterate instr (n instrs)
                                 (save-pos (cur vec)
                                    (setf (gethash n hash) (- cur begin-instrs))
                                    (output-instrs instrs vec)
                                    (save-pos (end vec)
                                       (setf (gethash n end-hash) (- end +code-offset-size+))))))
                           (save-pos (end-select vec)
                              (loop for i from 0 to (1- total-nodes)
                                    for pos = (* i +code-offset-size+)
                                    do (when-let ((offset (gethash i hash)))
                                          ;; offset is always one more, since when 0 it means there is
                                          ;; no code for the corresponding node
                                          (write-offset vec (1+ offset) (+ start size-header pos)))
                                    do (when-let ((write-end (gethash i end-hash)))
                                          (write-offset vec (- end-select (1- write-end)) write-end)))
                              (write-offset vec (- end-select (1- start)) start)))))
      (:return-select (add-byte #b00001011 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec))
      (:colocated
         (do-vm-values vec ((vm-colocated-first instr) (vm-colocated-second instr))
            #b00001100
            (logand *value-mask* first-value)
            (logand *value-mask* second-value)
            (logand *reg-mask* (reg-to-byte (vm-colocated-dest instr)))))
      (:rule
         (add-byte #b00010000 vec)
         (dolist (b (output-int (vm-rule-id instr)))
            (add-byte b vec)))
      (:rule-done
         (add-byte #b00010001 vec))
		(:new-node
			(add-byte #b00010011 vec)
			(add-byte (logand *reg-mask* (reg-num (vm-new-node-reg instr))) vec))
      (:delete (let* ((filters (vm-delete-filter instr)))
                  (add-byte #b00001101 vec)
                  (add-byte (logand *tuple-id-mask* (lookup-def-id (vm-delete-name instr))) vec)
                  (add-byte (length filters) vec)
                  (dolist (filter filters)
                     (let* ((ind (car filter))
                            (val (cdr filter))
                            (res (output-value val)))
                        (add-byte (1- ind) vec)
                        (add-byte (first res) vec)
                        (add-bytes vec (second res))))))
      (otherwise (error 'output-invalid-error :text (tostring "Unknown instruction to output ~a" instr)))))
                
(defun output-instrs (ls vec)
   (dolist (instr ls)
      (output-instr instr vec)))
                             
(defun output-processes ()
   (do-processes (:name name :instrs instrs :operation collect)
      (printdbg "Processing predicate ~a..." name)
      (letret (vec (create-bin-array))
         (output-instrs instrs vec))))

(defun type-to-byte (typ)
	(case typ
      (:type-int #b0000)
      (:type-float #b0001)
      (:type-addr #b0010)
      (:type-list-int #b0011)
      (:type-list-float #b0100)
      (:type-list-addr #b0101)
      (:type-worker #b0110)
		(:type-string #b1001)
      (otherwise (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))

(defun output-arg-type (typ vec)
   (add-byte (type-to-byte typ) vec))

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
               (:type-float #b0111)
               (:type-list-float #b1011)))))

(defun output-aggregate (types)
   (let ((agg (find-if #'aggregate-p types)))
      (if agg
         (let ((pos (position-if #'aggregate-p types))
               (agg (aggregate-agg agg))
               (typ (aggregate-type agg)))
            (logior (logand #b11110000 (ash (output-aggregate-type agg typ) 4))
                    (logand #b00001111 pos)))
         #b00000000)))

(defun output-properties (def)
   (letret (prop #b00000000)
      (when (definition-aggregate def)
         (setf prop (logior prop #b00000001)))
      (when (is-reverse-route-p def)
         (setf prop (logior prop #b00000100)))
      (when (is-route-p def)
         (setf prop (logior prop #b00000010)))
      (when (is-linear-p def)
         (setf prop (logior prop #b00001000)))
      (when (is-action-p def)
         (setf prop (logior prop #b00010000)))
		(when (is-reused-p def)
			(setf prop (logior prop #b00100000)))
      prop))

(defparameter *max-tuple-name* 32)
(defparameter *max-tuple-args* 32)
(defparameter *max-agg-info* 32)

(defmacro refill-up-to ((vec max) &body body)
   (with-gensyms (pos new-pos)
      `(save-pos (,pos ,vec)
         ,@body
         (save-pos (,new-pos ,vec)
            (loop for i from (1+ (- ,new-pos ,pos)) to ,max
                  do (add-byte #b0 ,vec))))))

(defun output-tuple-name (name vec)
   (refill-up-to (vec *max-tuple-name*)
      (loop for x being the elements of name
         do (add-byte (char-code x) vec))))
         
(defun output-tuple-type-args (types vec)
   (refill-up-to (vec *max-tuple-args*)
      (dolist (typ (definition-arg-types types))
         (output-arg-type typ vec))))
         
(defun output-stratification-level (def)
   (let ((level (definition-get-tagged-option def :strat)))
      (assert (not (null level)))
      (coerce level '(unsigned-byte 8))))
      
(defun get-aggregate-remote (def)
   "Gets remote aggregate info from a definition (only applicable to aggregates)."
   (when-let ((agg (definition-aggregate def)))
      (let ((mod (aggregate-mod agg)))
         (cond
            ((and *use-stratification* (aggregate-mod-is-input-p mod))
               (values (generate-inverse-name (aggregate-mod-io-name mod))
                       (aggregate-mod-includes-home-p mod)))
            ((and *use-stratification* (aggregate-mod-is-output-p mod))
               (values (aggregate-mod-io-name mod)
                       (aggregate-mod-includes-home-p mod)))))))
      
(defconstant +agg-local-aggregate-byte+         #b00000001)
(defconstant +agg-remote-aggregate-byte+        #b00000010)
(defconstant +agg-remote-aggregate-home-byte+   #b00000100)
(defconstant +agg-immediate-aggregate-byte+     #b00001000)
(defconstant +agg-unsafe-byte+                  #b00000000)

(defun output-aggregate-info (def vec)
   (refill-up-to (vec *max-agg-info*)
      (let ((agg (definition-aggregate def)))
               (when agg
                  (cond
                     ((definition-has-local-agg-p def)
                        (add-byte +agg-local-aggregate-byte+ vec)
                        (let ((level (definition-get-strata def)))
                           (assert level)
                           ;(format t "local agg ~a ~a~%" (definition-name def) (1 level))
                           (add-byte (1+ level) vec)))
                     (t
                        (let ((aggmod (aggregate-mod agg)))
                           (cond
                              ((or (aggregate-mod-is-input-p aggmod)
                                    (aggregate-mod-is-output-p aggmod))
                                 (multiple-value-bind (remote-pred use-home-p) (get-aggregate-remote def)
                                    (when remote-pred
                                       (add-byte (if use-home-p +agg-remote-aggregate-home-byte+ +agg-remote-aggregate-byte+) vec)
                                       (add-byte (lookup-def-id remote-pred) vec))))
                              ((aggregate-mod-is-immediate-p aggmod)
                                 (printdbg "THIS PROGRAM HAS IMMEDIATE AGGREGATES!")
                                 (add-byte +agg-immediate-aggregate-byte+ vec))
                              (t (printdbg "THIS PROGRAM HAS UNSAFE AGGREGATES!")
                                 (add-byte +agg-unsafe-byte+ vec))))))))))

(defun output-descriptors ()
   (do-definitions (:definition def :name name :types types :operation collect)
      (letret (vec (create-bin-array))
         (add-byte (output-properties def) vec) ; property byte
         (add-byte (output-aggregate types) vec) ; aggregate byte
         (add-byte (output-stratification-level def) vec)
         (add-byte (length types) vec) ; number of args
         (output-tuple-type-args types vec) ; argument type information
         (output-tuple-name name vec) ;; predicate name
         (output-aggregate-info def vec) ;; aggregate info
      )))

(defun output-consts ()
	(letret (vec (create-bin-array))
		(output-instrs *consts-code* vec)))
            
(defparameter *total-written* 0)

(defun write-hexa (stream int) (incf *total-written*) (write-byte int stream))
(declaim (inline write-hexa))

(defun write-vec (stream vec)
   (write-sequence vec stream)
   (incf *total-written* (length vec)))
      
(defun write-short-stream (stream int)
   (let ((ls (output-int int)))
      (write-hexa stream (first ls))
      (write-hexa stream (second ls))))

(defun write-int-stream (stream int)
   (dolist (part (output-int int))
      (write-hexa stream part)))

(defun write-string-stream (stream str)
   (loop for x being the elements of str
      do (write-hexa stream (char-code x))))

(defun write-float-stream (stream flt)
	(dolist (part (output-float flt))
		(write-hexa stream part)))

(defun write-nodes (stream nodes)
   (when (zerop (number-of-nodes nodes))
      (format t "WARNING: there are no nodes defined in this program~%"))
   (write-int-stream stream (number-of-nodes nodes))
   (iterate-nodes (fake-id real-id nodes)
      (write-int-stream stream fake-id)
      (write-int-stream stream real-id)))

(defun write-priority-order (stream order)
	(case order
		(:asc (write-hexa stream #b00000001))
		(:desc (write-hexa stream #b00000000))))

(defun output-global-priority (stream)
	(write-hexa stream 1)
	(let* ((found (find-if #'global-priority-p *priorities*))
			 (id (lookup-def-id (global-priority-name found))))
		(write-hexa stream (logand *tuple-id-mask* id))
		(write-short-stream stream (global-priority-argument found))
		(write-priority-order stream (global-priority-asc-desc found))))

(defun any-global-priority-p ()
	(let ((found (find-if #'global-priority-p *priorities*)))
		(ensure-bool found)))
		
(defun get-initial-priority ()
	(let ((found (find-if #'initial-priority-p *priorities*)))
		(when found
			(initial-priority-value found))))
			
(defun output-initial-priority (stream)
	"Writes priority information."
	(write-hexa stream 2)
	(write-hexa stream 1) ; float
	(let ((order (find-if #'priority-order-p *priorities*)))
		(cond
			(order
				(write-priority-order stream (priority-order order)))
			(t
				(write-priority-order stream :desc))))
	(let ((prio (get-initial-priority)))
		(assert (not (null prio)))
		(write-float-stream stream prio)))
			
(defun write-rules (stream)
   (write-int-stream stream (1+ (length *clauses*)))
	(let ((init-rule-str "init -o axioms"))
		(write-int-stream stream (length init-rule-str))
		(write-string-stream stream init-rule-str))
   (do-rules (:clause clause)
      (let ((str (clause-to-string clause)))
         (write-int-stream stream (length str))
         (write-string-stream stream str))))

(defun output-all-rules ()
	(loop for code-rule in *code-rules*
		collect
   		(let ((vec (create-bin-array))
			 		(code (rule-code code-rule)))
				(output-instrs code vec)
				vec)))
			
(defun do-output-code (stream)
   (write-hexa stream (length *definitions*))
   (write-nodes stream *nodes*)
	(write-hexa stream (args-needed *ast*))
   (write-rules stream)
   (let* ((*output-string-constants* nil)
			 (processes (output-processes))
          (descriptors (output-descriptors))
			 (rules (output-all-rules))
			 (consts (output-consts)))
		;; output strings
		(write-int-stream stream (length *output-string-constants*))
		(loop for str in *output-string-constants*
				for i from 0
				do (write-int-stream stream (length str))
				do (write-vec stream (output-string str)))
		; output constant code
		(write-int-stream stream (length *consts*))
		(do-constant-list *consts* (:type typ)
			(write-hexa stream (type-to-byte typ)))
		(write-int-stream stream (length consts))
		(write-vec stream consts)
		; output predicate descriptions
      (loop for vec-desc in descriptors
            for vec-proc in processes
            do (write-int-stream stream (length vec-proc)) ; write code size first
            do (write-vec stream vec-desc))
		; output global priority predicate, if any
		(cond
			((any-global-priority-p)
				(output-global-priority stream))
			((get-initial-priority)
				(output-initial-priority stream))
			(t (write-hexa stream 0)))
      (dolist (vec processes) (write-vec stream vec))
		(write-int-stream stream (length *code-rules*))
		(dolist2 (vec rules) (code-rule *code-rules*)
		 (let* ((ids (subgoal-ids code-rule))
				 (pers-p (persistent-p code-rule)))
			(write-int-stream stream (length vec))
			
			(write-vec stream vec)
			
			(if pers-p
				(write-hexa stream 1)
				(write-hexa stream 0))
			
			(write-int-stream stream (length ids))
			(dolist (id ids)
				(write-hexa stream id))
			))))
   
(defmacro with-output-file ((stream file) &body body)
   `(with-open-file (,stream ,file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      ,@body))

(defun output-code (file &key (write-ast nil) (write-code nil))
   (let ((*total-written* 0)
         (byte-file (concatenate 'string file ".m")))
      (with-open-file (stream byte-file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :element-type '(unsigned-byte 8))
         (do-output-code stream))
      (when write-ast
         (with-output-file (stream (concatenate 'string file ".m.ast"))
            (format stream "~a~%" *ast*)))
      (when write-code
         (with-output-file (stream (concatenate 'string file ".m.code"))
            (format stream "~a" (print-vm))))))
