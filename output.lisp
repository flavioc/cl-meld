
(in-package :cl-meld)

(define-condition output-invalid-error (error)
   ((text :initarg :text :reader text)))

(defparameter *value-mask* #b00111111)
(defparameter *reg-mask* #b00011111)
(defparameter *op-mask* #b00011111)
(defparameter *tuple-id-mask* #b01111111)
(defparameter *extern-id-mask* #b01111111)

(defparameter *output-string-constants* nil)
(defun push-string-constant (str)
	"Adds string constant to database if not found and returns the string integer code."
	(let ((pos (position str *output-string-constants* :test #'string-equal)))
		(if pos
			pos
			(progn
				(push-end str *output-string-constants*)
				(1- (length *output-string-constants*))))))
				
(defparameter *program-types* nil)

(defmacro with-memory-stream (s &body body)
   `(let ((,s (make-in-memory-output-stream)))
      ,@body
      s))
   
(defun output-reg-dot (rd)
	(list (reg-dot-field rd) (reg-num (reg-dot-reg rd))))
	
(defun output-int (int)
   (loop for i upto 3
      collect (ldb (byte 8 (* i 8)) int)))
(defun output-int64 (int)
	(loop for i upto 7
		collect (ldb (byte 8 (* i 8)) int)))
(defun output-float32 (flt) (output-int (encode-float32 (coerce flt 'float))))
(defun output-float64 (flt) (output-int64 (encode-float64 (coerce flt 'float))))
(defun output-float (flt) (output-float64 flt))
(defun output-string (str)
	(map 'list #'char-code str))
(defun output-addr (addr)
	(output-int64 (vm-addr-num addr)))
	
(defconstant +any-value-flag+ #b001111)
	
(defun output-list (ls)
	(let ((head (vm-list-head ls))
			 (tail (vm-list-tail ls)))
		(let ((bytes-head (output-value head))
				(bytes-tail (output-value tail)))
			(append (flatten bytes-head) (flatten bytes-tail)))))
			
(defun variable-value-p (val)
	(cond
		((reg-p val) t)
		((reg-dot-p val) t)
		((vm-pcounter-p val) t)
		((vm-stack-p val) t)
		((vm-list-p val)
			(or (variable-value-p (vm-list-head val))
				(variable-value-p (vm-list-tail val))))
		(t nil)))

(defun output-value (val)
   (cond
		((vm-any-p val) (list +any-value-flag+))
		((vm-bool-p val) (list #b001100 (list (if (vm-bool-val val) #b1 #b0))))
      ((vm-int-p val) (list #b000001 (output-int (vm-int-val val))))
      ((vm-float-p val) (list #b000000 (output-float (vm-float-val val))))
		((vm-list-p val) (list #b001110 (output-list val)))
		((vm-string-constant-p val)
			(let* ((str (vm-string-constant-val val))
					 (code (push-string-constant str)))
				(list #b000110 (output-int code))))
      ((vm-addr-p val) (list  #b000101 (output-addr val)))
      ((vm-ptr-p val) (list #b001011 (output-int64 (vm-ptr-val val))))
		((vm-host-id-p val) (list #b000011))
      ((vm-nil-p val) (list #b000100))
		;; special value to handle matches with non-nil lists
		((vm-non-nil-p val) (list #b001101))
      ((vm-world-p val) (output-value (make-vm-int (number-of-nodes *nodes*))))
      ((vm-pcounter-p val) (list #b001010))
		((vm-stack-p val) (list #b001001 (list (vm-stack-offset val))))
      ((reg-p val) (list (logior #b100000 (logand #b011111 (reg-num val)))))
      ((reg-dot-p val) (list #b000010 (output-reg-dot val)))
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

(defun output-values (vec vals)
	(loop for val in vals
			do
				(if (reg-p val)
					(add-byte (reg-to-byte val) vec)
					(output-list-bytes vec (second (output-value val))))))
					
(defun output-instr-and-values-extra (vec instr extra-bytes vals)
	(add-byte instr vec)
	(output-list-bytes vec extra-bytes)
	(output-values vec vals))
	
(defun output-instr-and-values (vec instr &rest vals)
	(output-instr-and-values-extra vec instr nil vals))
	
(defun output-instr-type-and-values (vec instr type &rest vals)
	(output-instr-and-values-extra vec instr (list (lookup-type-id type)) vals))
	
(defun output-instr-index-and-values (vec instr idx &rest vals)
	(output-instr-and-values-extra vec instr (list idx) vals))
	
(defun output-call (vec call instr &optional extra-bytes)
	(let ((extern-id (lookup-external-function-id (vm-call-name call)))
             (args (vm-call-args call)))
         (add-byte instr vec)
         (add-byte (logand *extern-id-mask* extern-id) vec)
         (add-byte (logand *reg-mask* (reg-to-byte (vm-call-dest call))) vec)
			(output-list-bytes vec extra-bytes)
         (output-values vec args)))

(defun output-calle (vec call instr &optional extra-bytes)
	(let ((extern-id (lookup-custom-external-function-id (vm-calle-name call)))
             (args (vm-call-args call)))
         (add-byte instr vec)
         (add-byte (logand *extern-id-mask* extern-id) vec)
         (add-byte (logand *reg-mask* (reg-to-byte (vm-call-dest call))) vec)
			(output-list-bytes vec extra-bytes)
         (output-values vec args)))
      
(defun reg-to-byte (reg) (reg-num reg))
      
(defun lookup-extern-id (ast extern)
   (do-externs ast (:id id :name name)
      (if (string-equal name extern) (return-from lookup-extern-id id))))

(defun lookup-const-id (const)
	(do-constant-list *consts* (:name name :id id)
		(if (string-equal name const) (return-from lookup-const-id id))))
		
(defun lookup-function-id (name)
	(do-functions *functions* (:name fun-name :id id)
		(if (string-equal name fun-name)
			(return-from lookup-function-id id))))

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
(defun output-list-bytes (vec ls)
	(dolist (b ls)
      (add-byte b vec)))

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
         
(defun output-axiom-argument (arg vec subgoal)
	(cond
		((addr-p arg) (output-list-bytes vec (output-addr arg)))
		((int-p arg)
			(if (type-float-p (expr-type arg))
				(output-list-bytes vec (output-float (int-val arg)))
				(output-list-bytes vec (output-int (int-val arg)))))
		((float-p arg) (output-list-bytes vec (output-float (float-val arg))))
		((string-constant-p arg) (output-list-bytes vec (output-int (push-string-constant (string-constant-val arg)))))
		((nil-p arg) (add-byte #b0 vec))
		((cons-p arg)
			(add-byte #b1 vec)
			(output-axiom-argument (cons-head arg) vec subgoal)
			(output-axiom-argument (cons-tail arg) vec subgoal))
		(t (error 'output-invalid-error :text (tostring "don't know how to output this subgoal: ~a" subgoal)))))

(defun constant-matches-p (iter-matches)
	(loop for match in iter-matches
			do (let ((val (match-right match)))
					(when (variable-value-p val)
						(return-from constant-matches-p nil))))
	t)

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
		(when (constant-matches-p (iterate-matches iter))
			(setf opt (logior opt #b00001000)))
		(values opt snd)))

(defun output-instr (instr vec)
   (case (instr-type instr)
      (:return (add-byte #x0 vec))
      (:next (add-byte #x1 vec))
      (:return-linear (add-byte #b11010000 vec))
      (:return-derived (add-byte #b11110000 vec))
		(:new-axioms
			(write-jump vec 1
				(add-byte #b00010100 vec)
				(jumps-here vec)
				(let ((axioms (vm-new-axioms-subgoals instr)))
					(do-subgoals axioms (:name name :args args :subgoal axiom)
						(add-byte (logand *tuple-id-mask* (lookup-def-id name)) vec)
						(dolist (arg args)
							(output-axiom-argument arg vec axiom))))))
		(:send-delay
			(add-byte #b00010101 vec)
			(add-byte (logand *reg-mask* (reg-to-byte (vm-send-delay-from instr))) vec)
         (add-byte (logand *reg-mask* (reg-to-byte (vm-send-delay-to instr))) vec)
			(output-list-bytes vec (output-int (vm-send-delay-time instr))))
      (:reset-linear
         (write-jump vec 1
            (add-byte #b00001110 vec)
            (jumps-here vec)
            (output-instrs (vm-reset-linear-instrs instr) vec)))
		(:end-linear
			(add-byte #b00001111 vec))
		(:push
			(add-byte #b00010110 vec))
		(:pop
			(add-byte #b00010111 vec))
		(:push-registers
			(add-byte #b00011000 vec))
		(:pop-registers
			(add-byte #b00011001 vec))
      (:remove
            (let ((reg (vm-remove-reg instr)))
               (add-byte #b10000000 vec)
               (add-byte (logand *reg-mask* (reg-to-byte reg)) vec)))
      (:alloc (let ((tuple-id (lookup-def-id (vm-alloc-tuple instr)))
                    (reg (reg-to-byte (vm-alloc-reg instr))))
                  (add-byte #b01000000 vec)
                  (add-byte (logand *tuple-id-mask* tuple-id) vec)
                  (add-byte (logand *reg-mask* reg) vec)))
      (:send (add-byte #b00001000 vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-from instr))) vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-to instr))) vec))
		(:callf (add-byte #b00011010 vec)
				  (add-byte (lookup-function-id (vm-callf-name instr)) vec))
      (:call
			(output-call vec instr #b00100000 (list (length (vm-call-args instr)))))
		(:calle
			(output-calle vec instr #b00011011 (list (length (vm-calle-args instr)))))
      (:if (let ((reg-b (reg-to-byte (vm-if-reg instr))))
             (write-jump vec 2
               (add-byte #b01100000 vec)
               (add-byte (logand *reg-mask* reg-b) vec)
               (jumps-here vec)
               (output-instrs (vm-if-instrs instr) vec))))
      (:iterate (write-jump vec 17 ;; outside jump
						(write-jump vec 13 ;; inner jump
                  	(add-byte #b10100000 vec)
							(loop for i from 1 upto 8
								do (add-byte #b0 vec))
                  	(add-byte (lookup-def-id (iterate-name instr)) vec)
							(add-byte (logand *reg-mask* (reg-to-byte (iterate-reg instr))) vec)
							(multiple-value-bind (b1 b2) (iterate-options-bytes instr)
								(add-byte b1 vec)
								(add-byte b2 vec))
                  	(jumps-here vec)
							(jumps-here vec)
                  	(output-matches (iterate-matches instr) vec))
                  (output-instrs (iterate-instrs instr) vec)
                  (add-byte #b00000001 vec)))
		(:move
				(do-vm-values vec ((move-from instr) (move-to instr))
                #b00110000
                (logand *value-mask* first-value)
                (logand *value-mask* second-value)))
		(:move-int-to-field
			(output-instr-and-values vec #b00011110 (move-from instr) (move-to instr)))
		(:move-int-to-reg
			(output-instr-and-values vec #b00011111 (move-from instr) (move-to instr)))
		(:move-field-to-field
			(output-instr-and-values vec #b00100001 (move-from instr) (move-to instr)))
		(:move-field-to-reg
			(output-instr-and-values vec #b00100010 (move-from instr) (move-to instr)))
		(:move-ptr-to-reg
			(output-instr-and-values vec #b00100011 (move-from instr) (move-to instr)))
      (:move-nil-to-field
			(output-instr-and-values vec #b01110000 (move-to instr)))
		(:move-nil-to-reg
			(output-instr-and-values vec #b00100100 (move-to instr)))
		(:move-field-to-field-ref
			(output-instr-and-values vec #b00100101 (move-from instr) (move-to instr)))
		(:move-reg-to-field
			(output-instr-and-values vec #b00100110 (move-from instr) (move-to instr)))
		(:move-reg-to-field-ref
			(output-instr-and-values vec #b00100111 (move-from instr) (move-to instr)))
		(:move-host-id-to-field
			(output-instr-and-values vec #b00101000 (move-from instr) (move-to instr)))
		(:move-reg-to-constant
			(output-instr-and-values vec #b00101001 (move-from instr) (move-to instr)))
		(:move-constant-to-field
			(output-instr-and-values vec #b00101010 (move-from instr) (move-to instr)))
		(:move-constant-to-field-ref
			(output-instr-and-values vec #b00101011 (move-from instr) (move-to instr)))
		(:move-addr-to-field
			(output-instr-and-values vec #b00101100 (move-from instr) (move-to instr)))
		(:move-float-to-field
			(output-instr-and-values vec #b00101101 (move-from instr) (move-to instr)))
		(:move-float-to-reg
			(output-instr-and-values vec #b00101110 (move-from instr) (move-to instr)))
		(:move-int-to-constant
			(output-instr-and-values vec #b00101111 (move-from instr) (move-to instr)))
		(:move-world-to-field
			(output-instr-and-values vec #b00110001 (move-to instr)))
		(:move-stack-to-pcounter
			(output-instr-and-values vec #b00110010 (move-from instr)))
    	(:move-pcounter-to-stack
			(output-instr-and-values vec #b00110011 (move-to instr)))
		(:move-stack-to-reg
			(output-instr-and-values vec #b00110100 (move-from instr) (move-to instr)))
		(:move-reg-to-stack
			(output-instr-and-values vec #b00110101 (move-from instr) (move-to instr)))
		(:move-addr-to-reg
			(output-instr-and-values vec #b00110110 (move-from instr) (move-to instr)))
		(:move-host-id-to-reg
			(output-instr-and-values vec #b00110111 (move-to instr)))
		(:addr-not-equal
			(output-instr-and-values vec #b00111000 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:addr-equal
			(output-instr-and-values vec #b00111001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-minus
			(output-instr-and-values vec #b00111010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-equal
			(output-instr-and-values vec #b00111011 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-not-equal
			(output-instr-and-values vec #b00111100 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-plus
			(output-instr-and-values vec #b00111101 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-lesser
			(output-instr-and-values vec #b00111110 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-greater-equal
			(output-instr-and-values vec #b00111111 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:bool-or
			(output-instr-and-values vec #b01000001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-lesser-equal
			(output-instr-and-values vec #b01000010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-greater
			(output-instr-and-values vec #b01000011 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-mul
			(output-instr-and-values vec #b01000100 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-div
			(output-instr-and-values vec #b01000101 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-plus
			(output-instr-and-values vec #b01000110 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-minus
			(output-instr-and-values vec #b01000111 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-mul
			(output-instr-and-values vec #b01001000 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-div
			(output-instr-and-values vec #b01001001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-equal
			(output-instr-and-values vec #b01001010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-not-equal
			(output-instr-and-values vec #b01001011 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-lesser
			(output-instr-and-values vec #b01001100 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-lesser-equal
			(output-instr-and-values vec #b01001101 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-greater
			(output-instr-and-values vec #b01001110 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-greater-equal
			(output-instr-and-values vec #b01001111 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:move-reg-to-reg
			(output-instr-and-values vec #b01010000 (move-from instr) (move-to instr)))
  		(:test-nil
			(output-instr-and-values vec #b00000011 (vm-test-nil-place instr) (vm-test-nil-dest instr)))
      (:not
			(output-instr-and-values vec #b00000111 (vm-not-place instr) (vm-not-dest instr)))
		(:bool-equal
			(output-instr-and-values vec #b01010001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:bool-not-equal
			(output-instr-and-values vec #b01010010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:head-rr
			(output-instr-and-values vec #b01010011 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-fr
			(output-instr-and-values vec #b01010100 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-ff
			(output-instr-and-values vec #b01010101 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-rf
			(output-instr-and-values vec #b01010110 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-ffr
			(output-instr-and-values vec #b01010111 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-rfr
			(output-instr-and-values vec #b01011000 (vm-head-cons instr) (vm-head-dest instr)))
		(:tail-rr
			(output-instr-and-values vec #b01011001 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:tail-fr
			(output-instr-and-values vec #b01011010 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:tail-ff
			(output-instr-and-values vec #b01011011 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:tail-rf
			(output-instr-and-values vec #b01011100 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:move-world-to-reg
			(output-instr-and-values vec #b01011101 (move-to instr)))
		(:move-constant-to-reg
			(output-instr-and-values vec #b01011110 (move-from instr) (move-to instr)))
		(:cons-rrr
			(output-instr-type-and-values vec #b01011111 (vm-cons-type instr) (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-rff
			(output-instr-and-values vec #b01100001 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-frf
			(output-instr-and-values vec #b01100010 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-ffr
			(output-instr-and-values vec #b01100011 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-rrf
			(output-instr-and-values vec #b01100100 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-rfr
			(output-instr-and-values vec #b01100101 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-frr
			(output-instr-type-and-values vec #b01100110 (vm-cons-type instr) (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-fff
			(output-instr-and-values vec #b01100111 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:call0
			(output-call vec instr #b01101000))
		(:call1
			(output-call vec instr #b01101001))
		(:call2
			(output-call vec instr #b01101010))
		(:call3
			(output-call vec instr #b01101011))
		(:move-int-to-stack
			(output-instr-and-values vec #b01101100 (move-from instr) (move-to instr)))
		(:push-n
			(add-byte #b01101101 vec)
			(add-byte (vm-push-n instr) vec))
		(:structr
			(output-instr-type-and-values vec #b00011101 (vm-make-struct-type instr) (vm-make-struct-to instr)))
		(:structf
			(output-instr-and-values vec #b01101110 (vm-make-struct-to instr)))
		(:struct-valrr
			(output-instr-index-and-values vec #b01101111 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valfr
			(output-instr-index-and-values vec #b01110001 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valrf
			(output-instr-index-and-values vec #b01110010 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valrf-ref
			(output-instr-index-and-values vec #b01110011 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valff
			(output-instr-index-and-values vec #b01110100 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valff-ref
			(output-instr-index-and-values vec #b01110101 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:move-float-to-stack
			(output-instr-and-values vec #b01110110 (move-from instr) (move-to instr)))
      (:convert-float
			(output-instr-and-values vec #b00001001 (vm-convert-float-place instr) (vm-convert-float-dest instr)))
      (:select-node
							(when (vm-select-node-empty-p instr)
								(return-from output-instr nil))
							(let* ((total-nodes (number-of-nodes *nodes*))
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
			(output-list-bytes vec (output-int (vm-rule-id instr))))
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
      (letret (vec (create-bin-array))
         (output-instrs instrs vec))))

(defun lookup-type-id (typ)
	(let ((ret (position typ *program-types* :test #'equal)))
		(assert (integerp ret))
		ret))

(defun type-to-bytes (typ)
	(cond
		((symbolp typ)
			(case typ
      		(:type-int '(#b0000))
      		(:type-float '(#b0001))
      		(:type-addr '(#b0010))
				(:type-bool '(#b0101))
				(:type-string '(#b1001))
				(otherwise (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))
		((type-list-p typ)
			(let* ((sub (type-list-element typ))
					 (bytes (type-to-bytes sub)))
				`(,#b0011 ,@bytes)))
		((type-struct-p typ)
			(let ((ls (type-struct-list typ)))
				(let ((x `(,#b0100 ,(length ls) ,@(loop for ty in ls append (type-to-bytes ty)))))
					x)))
		(t (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))

(defun output-arg-type (typ vec)
	(add-byte (lookup-type-id typ) vec))
		
(defun write-arg-type (stream typ)
	(write-hexa stream (lookup-type-id typ)))
		
(defun output-aggregate-type (agg typ)
   (case agg
      (:first #b0001)
      (:min (case typ
               (:type-int #b0011)
               (:type-float #b0110)))
      (:max (case typ
               (:type-int #b0010)
               (:type-float #b0101)))
      (:sum (cond
					((type-int-p typ) #b0100)
					((type-float-p typ) #b0111)
					((type-list-p typ)
						(let ((sub (type-list-element typ)))
							(cond
								((type-float-p sub) #b1011))))))))

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
		(when (definition-is-cyclical-p def)
			(setf prop (logior prop #b01000000)))
      prop))

(defparameter *max-tuple-name* 32)
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
	(dolist (typ (definition-arg-types types))
		(output-arg-type typ vec)))
         
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

(defun output-functions ()
	(printdbg "Processing functions")
	(loop for code in *function-code*
		collect (letret (vec (create-bin-array))
						(output-instrs code vec))))
   
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

(defun write-list-stream (stream bytes)
	(dolist (b bytes)
		(write-hexa stream b)))

(defun write-int-stream (stream int)
	(write-list-stream stream (output-int int)))

(defun write-int64-stream (stream int)
	(write-list-stream stream (output-int64 int)))

(defun write-string-stream (stream str)
   (loop for x being the elements of str
      do (write-hexa stream (char-code x))))

(defun write-float-stream (stream flt)
	(write-list-stream stream (output-float flt)))

(defun write-nodes (stream nodes)
   (when (zerop (number-of-nodes nodes))
      (format t "WARNING: there are no nodes defined in this program~%"))
   (write-int-stream stream (number-of-nodes nodes))
   (iterate-nodes (fake-id real-id nodes)
      (write-int64-stream stream fake-id)
      (write-int64-stream stream real-id)))
		
(defun priority-order-bit (order)
	(case order
		(:asc #b00000001)
		(:desc #b00000000)))
		
;;
;; the following 3 functions output the main byte-code options
;;

(defun output-initial-priority (stream)
	"Writes priority information."
	(write-hexa stream 2)
	(write-hexa stream 1) ; float
	(let* ((order (find-if #'priority-order-p *priorities*))
			 (static (find-if #'priority-static-p *priorities*))
			 (byt (priority-order-bit (if order (priority-order order) :desc))))
		(when static
			(setf byt (logior byt #b00000010)))
		(write-hexa stream byt))
	(let ((prio (get-initial-priority)))
		(write-float-stream stream (if prio prio 0.0))))
		
(defun output-data-file-info (stream)
	(write-hexa stream 3))

(defun get-initial-priority ()
	(let ((found (find-if #'initial-priority-p *priorities*)))
		(when found
			(initial-priority-value found))))
			
(defun write-rules (stream)
   (write-int-stream stream (1+ (length *clauses*)))
	(let ((init-rule-str "init -o axioms"))
		(write-int-stream stream (length init-rule-str))
		(write-string-stream stream init-rule-str))
   (do-rules (:clause clause)
      (let ((str (clause-to-string clause)))
         (write-int-stream stream (length str))
         (write-string-stream stream str))))

(defun output-all-rules (&optional (is-data-p nil))
	(printdbg "Processing rules...")
	(loop for code-rule in *code-rules*
			for count = 0 then (1+ count)
			collect
   			(let ((vec (create-bin-array))
			 			(code (rule-code code-rule)))
					(if (and (= count 0) is-data-p)
						;; for data files just print the select by node instruction
						(let* ((iterate (second (rule-code code-rule)))
								(select (find-if #'(lambda (instr) (eq (first instr) :select-node)) (iterate-instrs iterate))))
							(output-instrs `(,(make-vm-rule 0) ,select ,(make-return-derived)) vec))
						(output-instrs code vec))
					vec)))
				
(defparameter +meld-magic+ '(#x6d #x65 #x6c #x64 #x20 #x66 #x69 #x6c))

(defun add-type-to-typelist (types new)
	(if (member new types :test #'equal)
		types
		(push new types)))
		
(defun collect-all-types ()
	(setf *program-types* nil)
	(do-definitions (:types types)
		(dolist (ty types)
			(setf *program-types* (add-type-to-typelist *program-types* (arg-type ty)))))
	(do-constant-list *consts* (:type typ)
		(setf *program-types* (add-type-to-typelist *program-types* typ)))
	(do-externs *externs* (:types types :ret-type ret)
		(setf *program-types* (add-type-to-typelist *program-types* ret))
		(dolist (typ types)
			(setf *program-types* (add-type-to-typelist *program-types* typ))))
	(do-functions *functions* (:ret-type typ :args args)
		(setf *program-types* (add-type-to-typelist *program-types* typ))
		(dolist (arg args)
			(setf *program-types* (add-type-to-typelist *program-types* (var-type arg))))))

(defun do-output-header (stream)
	(printdbg "Processing header...")
	(dolist (magic +meld-magic+)
		(write-hexa stream magic))
	(write-int-stream stream *major-version*)
	(write-int-stream stream *minor-version*)
	(write-hexa stream (length *definitions*))
   (write-nodes stream *nodes*)
	(collect-all-types)
	(write-hexa stream (length *program-types*))
	(loop for typ in *program-types*
			do (let ((bytes (type-to-bytes typ)))
					(write-list-stream stream bytes)))
	(write-int-stream stream (length *imported-predicates*))
	(do-imports *imported-predicates* (:imp imp :as as :from file)
		(write-int-stream stream (length imp))
		(write-vec stream (output-string imp))
		(write-int-stream stream (length as))
		(write-vec stream (output-string as))
		(write-int-stream stream (length file))
		(write-vec stream (output-string file)))
	(write-int-stream stream (length *exported-predicates*))
	(dolist (name *exported-predicates*)
		(write-int-stream stream (length name))
		(write-vec stream (output-string name))))

(defun do-output-descriptors (stream descriptors processes)
	(printdbg "Processing predicates...")
	(loop for vec-desc in descriptors
         for vec-proc in processes
         do (write-int-stream stream (length vec-proc)) ; write code size first
         do (write-vec stream vec-desc)))

(defun do-output-string-constants (stream)
	(write-int-stream stream (length *output-string-constants*))
	(loop for str in *output-string-constants*
			for i from 0
			do (write-int-stream stream (length str))
			do (write-vec stream (output-string str))))
			
(defun do-output-consts (stream consts)
	(printdbg "Processing constants...")
	(write-int-stream stream (length *consts*))
	(do-constant-list *consts* (:type typ)
		(write-arg-type stream typ))
	(write-int-stream stream (length consts))
	(write-vec stream consts))
	
(defun do-output-rules (stream rules)
	(printdbg "Processing rules...")
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
		)))
		
(defun do-output-functions (stream functions)
	(printdbg "Processing functions...")
	(write-int-stream stream (length functions))
	(dolist (fun functions)
		(write-int-stream stream (length fun))
		(write-vec stream fun)))
		
(defconstant +max-extern-function-name+ 256)
(defconstant +max-extern-file-name+ 1024)
		
(defun do-output-externs (stream)
	(write-int-stream stream (length *externs*))
	(do-externs *externs* (:name name :extern ex :types types :ret-type ret)
		(write-int-stream stream (extern-id ex))
		(let ((len (length name)))
			(write-string-stream stream name)
			(loop for i from len upto (1- +max-extern-function-name+)
				do (write-hexa stream 0)))
		(loop for i from 1 upto +max-extern-file-name+
			do (write-hexa stream 0))
		(write-int64-stream stream 0)
		(write-int-stream stream (length types))
		(write-arg-type stream ret)
		(dolist (typ types)
			(write-arg-type stream typ))))
			
(defun do-output-data (stream)
	(do-output-header stream)
	(when (> (args-needed *ast*) 0)
		(error 'output-invalid-error :text (tostring "Data files cannot have arguments")))
	(when (> (length *consts*) 0)
		(error 'output-invalid-error :text (tostring "Cannot have constants in data files")))
	(when (> (length *functions*) 0)
		(error 'output-invalid-error :text (tostring "Cannot have functions in data files")))
	(write-hexa stream 0) ;; 0 args needed
	(write-rules stream)
	(let* ((*output-string-constants* nil)
			 (descriptors (output-descriptors))
			 (processes (loop for desc in descriptors
								collect (list)))
			 (rules (output-all-rules t))
			 (functions (output-functions))
			 (consts (output-consts)))
		(do-output-string-constants stream)
		(do-output-consts stream consts)
		(do-output-functions stream functions)
		; write external definitions
		(do-output-externs stream)
		(do-output-descriptors stream descriptors processes)
		(output-data-file-info stream)
		;; write init-code
		(when (> (length rules) 1)
			(warn "Too many rules in data file"))
		(do-output-rules stream rules)
	))
	
(defun do-output-code (stream)
   (do-output-header stream)
	(write-hexa stream (args-needed *ast*))
   (write-rules stream)
   (let* ((*output-string-constants* nil)
			 (processes (output-processes))
          (descriptors (output-descriptors))
			 (rules (output-all-rules))
			 (consts (output-consts))
			 (functions (output-functions)))
		;; output strings
		(do-output-string-constants stream)
		; output constant code
		(do-output-consts stream consts)
		; write functions
		(do-output-functions stream functions)
		; write external definitions
		(do-output-externs stream)
		; output predicate descriptions
      (do-output-descriptors stream descriptors processes)
		; output global priority predicate, if any
		(output-initial-priority stream)
      (dolist (vec processes) (write-vec stream vec))
		(do-output-rules stream rules)))
   
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

(defun output-data-file (file)
	(let ((*total-written* 0)
			(byte-file (concatenate 'string file ".md")))
		(with-open-file (stream byte-file
								:direction :output
								:if-exists :supersede
								:if-does-not-exist :create
								:element-type '(unsigned-byte 8))
			(do-output-data stream))))
