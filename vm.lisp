(in-package :cl-meld)

(defparameter *num-regs* 32)

(defmacro do-type-conversion (op base-type)
   `(case ,op
      ,@(mapcar #L`(,(format-keyword "~a" !1) ,(format-keyword "~a-~a" base-type !1))
                  '(equal lesser lesser-equal greater greater-equal plus mul div mod))))

(defun set-type-to-op (typ-args typ-ret op)
   (declare (ignore typ-ret))
   (case typ-args
      (:type-int (do-type-conversion op int))
      (:type-float (do-type-conversion op float))))

(defun make-process (name instrs) (list :process name instrs))
(defun process-name (proc) (second proc))
(defun process-instrs (proc) (third proc))

(defun make-move (from to) `(:move ,from ,to))
(defun move-to (mv) (third mv))
(defun move-from (mv) (second mv))

(defun make-return () '(:return))

(defun instr-type (instr) (first instr))

(defun make-reg (n) `(:reg ,n))
(defun reg-p (r) (tagged-p r :reg))
(defun reg-num (r) (second r))

(defun make-reg-dot (reg field) `(:reg-dot ,reg ,field))
(defun reg-dot-reg (reg-dot) (second reg-dot))
(defun reg-dot-field (reg-dot) (third reg-dot))
(defun reg-dot-p (reg-dot) (tagged-p reg-dot :reg-dot))

(defun make-if (r instrs) (list :if r instrs))
(defun if-reg (i) (second i))
(defun if-instrs (i) (third i))

(defun make-set (dst v1 op v2) (list :set dst :to v1 op v2))
(defun set-destiny (st) (second st))
(defun set-v1 (st) (fourth st))
(defun set-op (st) (fifth st))
(defun set-v2 (st) (sixth st))
;SET reg 2 TO 0.1 INT EQUAL 3

(defun make-iterate (name matches instrs) (list :iterate name matches instrs))
(defun iterate-name (i) (second i))
(defun iterate-matches (i) (third i))
(defun iterate-instrs (i) (fourth i))

;ITERATE OVER fact MATCHING
;  (match).0=0.0
;  (match).1=0.1
;  (match).2=0.1
;...
;ENDIF

(defun make-vm-alloc (tuple reg) `(:alloc ,tuple ,reg))
(defun vm-alloc-tuple (alloc) (second alloc))
(defun vm-alloc-reg (alloc) (third alloc))

; ALLOC fact TO reg 4

(defun make-vm-int (int) `(:int ,int))
(defun vm-int-p (int) (tagged-p int :int))
(defun vm-int-val (int) (second int))

; MOVE 0.0 TO 4.0

(defun make-send (from to &optional (time 0)) `(:send ,from ,to ,time))
(defun make-send-self (reg &optional (time 0)) (make-send reg reg time))
(defun send-from (send) (second send))
(defun send-to (send) (third send))
(defun send-time (send) (fourth send))

; SEND reg 7 TO reg 7 IN 0ms

(defun tuple-p (tp) (eq tp :tuple))
(defun match-p (m) (eq m :match))

(defun print-place (place)
   (cond
      ((vm-int-p place) (tostring "~a" (vm-int-val place)))
      ((reg-p place) (tostring "reg ~a" (reg-num place)))
      ((reg-dot-p place)
         (tostring "~a.~a"
            (if (match-p (reg-dot-reg place))
               "(match)"
               (reg-num (reg-dot-reg place)))
            (reg-dot-field place)))
      ((tuple-p place) "tuple")))
      
(defmacro generate-print-op (basic-typs basic-ops &body body)
   `(on-top-level
      (defun print-op (op)
         (case op
            ,@(loop for typ in basic-typs
                  appending (mapcar #L`(,(format-keyword "~a-~a" typ !1)
                                          ,(substitute #\Space #\- (tostring "~A ~A" typ !1))) basic-ops))
            (otherwise ,@body)))))
            
(generate-print-op (int float) (equal lesser lesser-equal greater greater-equal plus minus mul div mod))

(defun print-instr-ls (instrs)
   (reduce #L(if !1 (concatenate 'string !1 (list #\Newline) (print-instr !2)) (print-instr !2))
                  instrs :initial-value nil))
                  
(defun print-match (m) (tostring "  ~a=~a~%" (print-place (first m)) (print-place (second m))))
(defun print-matches (matches)
   (reduce #L(concatenate 'string !1 (print-match !2)) matches :initial-value nil))

(defun print-instr (instr)
   (case (instr-type instr)
      (:return "RETURN")
      (:send (tostring "SEND ~a TO ~a IN ~ams" (print-place (send-from instr))
                  (print-place (send-to instr)) (send-time instr)))
      (:alloc (tostring "ALLOC ~a TO ~a" (vm-alloc-tuple instr) (print-place (vm-alloc-reg instr))))
      (:iterate (tostring "ITERATE OVER ~a MATCHING~%~a~a~%NEXT" (iterate-name instr)
                  (print-matches (iterate-matches instr)) (print-instr-ls (iterate-instrs instr)))) 
      (:set (tostring "SET ~a TO ~a ~a ~a" (print-place (set-destiny instr)) (print-place (set-v1 instr))
                                             (print-op (set-op instr))
                                             (print-place (set-v2 instr)))) 
      (:if (tostring "IF (~a) THEN~%~a~%ENDIF" (reg-num (if-reg instr)) (print-instr-ls (if-instrs instr))))
      (:move (tostring "MOVE ~a TO ~a" (print-place (move-from instr)) (print-place (move-to instr))))))

(defun print-vm (processls)
   (do-processes processls (:name name :instrs instrs)
      (format t "PROCESS ~a:~%" name)
      (dolist (instr instrs)
         (format t "~a~%" (print-instr instr)))
      (format t "~%")))