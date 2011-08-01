(in-package :cl-meld)

(define-condition expr-invalid-error (error)
   ((text :initarg :text :reader text)))

(defmacro define-makes (&rest symbs)
   `(on-top-level
      ,@(mapcar #'(lambda (sym)
         `(defun ,(intern (concatenate 'string "MAKE-" (symbol-name sym))) (a b c)
               (declare (ignore b))
               (list ,sym a c)))
            symbs)))

(define-makes :plus :minus :mul :mod :div
      :lesser :lesser-equal :greater :greater-equal
      :equal :assign :not-equal)
      
(defun make-call (name args) `(:call ,name ,args))
(defun call-name (call) (second call))
(defun call-args (call) (third call))

(defun make-cons (h ts) `(:cons ,h ,ts))
(defun cons-head (c) (second c))
(defun cons-tail (c) (third c))

(defun set-cons-head (cons head)
   (setf (second cons) head))
(defsetf cons-head set-cons-head)

(defun set-cons-tail (cons tail)
   (setf (third cons) tail))
(defsetf cons-tail set-cons-tail)

(defun make-head (c) `(:head ,c))
(defun head-list (c) (second c))

(defun set-head-list (head list)
   (setf (second head) list))
(defsetf head-list set-head-list)

(defun make-tail (c) `(:tail ,c))
(defun tail-list (c) (second c))

(defun set-tail-list (tail list)
   (setf (second tail) list))
(defsetf tail-list set-tail-list)

(defun make-true () '(:true))
(defun make-false () '(:false))

(defun make-not (expr) `(:not ,expr))
(defun not-expr (not) (second not))

(defun set-not-expr (not expr)
   (setf (second not) expr))
(defsetf not-expr set-not-expr)

(defun make-test-nil (expr) `(:test-nil ,expr))
(defun test-nil-expr (tn) (second tn))

(defun set-test-nil-expr (test expr)
   (setf (second test) expr))
(defsetf test-nil-expr set-test-nil-expr)

(defun make-nil () (list :nil))

(defun make-addr (num) (list :addr num :type-addr))
(defun addr-num (addr) (second addr))
(defun set-addr-num (addr new-num)
   (setf (second addr) new-num))
(defsetf addr-num set-addr-num)

(defun option-has-tag-p (opts opt) (some #L(tagged-p !1 opt) opts))

;; to take the home node from the head   
(defun head-host-node (head-list)
   (first (subgoal-args (first head-list))))
(defun clause-head-host-node (clause)
   (head-host-node (clause-head clause)))
   
;; to take the home node from the body
(defun body-host-node (body-list)
   "Returns the home body of a body list.
   Note that other things other than subgoals may be present"
   (do-subgoals body-list (:args args)
      (return-from body-host-node (first args))))
(defun clause-body-host-node (clause)
   (body-host-node (clause-body clause)))
   
(defun clause-host-node (clause)
   "Returns the host node of a clause.
   Looks first on the body and then on the head."
   (let ((host (clause-body-host-node clause)))
      (if host
         host
         (clause-head-host-node clause))))

(defun make-colocated (h1 h2)
   (list :colocated h1 h2))
(defun colocated-first (c) (second c))
(defun colocated-second (c) (third c))

(defun set-colocated-first (c new)
   (setf (second c) new))
(defsetf colocated-first set-colocated-first)

(defun set-colocated-second (c new)
   (setf (third c) new))
(defsetf colocated-second set-colocated-second)
      
(defun make-subgoal (name args)
   (if (equal name "colocated")
       (cond
          ((= (length args) 2) (make-constraint (make-colocated (first args) (second args))))
          (t (error 'expr-invalid-error "Colocated expression must have two arguments")))
       (list :subgoal name args)))
(defun make-var (var &optional typ) `(:var ,(if (stringp var) (str->sym var) var) ,@(if typ `(,typ) nil)))

(defun make-definition (name typs options) `(:definition ,name ,typs ,options))
(defun definition-p (def) (tagged-p def :definition))
(defun definition-name (def) (second def))
(defun definition-types (def) (third def))
(defun set-definition-types (def new-types)
   (setf (third def) new-types))
(defsetf definition-types set-definition-types)
(defun definition-options (def) (fourth def))
(defun definition-add-option (def opt) (push opt (fourth def)))
(defun definition-has-option-p (def opt)
   (has-elem-p (definition-options def) opt))
(defun definition-has-tagged-option-p (def opt)
   (some #L(tagged-p !1 opt) (definition-options def)))
(defun definition-get-tagged-option (def opt)
   (let ((res (find-if #L(tagged-p !1 opt) (definition-options def))))
      (when res
         (second res))))
(defun definition-add-tagged-option (def name &rest rest)
   (definition-add-option def `(,name ,@rest)))

(defun is-worker-definition-p (def)
   (definition-has-option-p def :worker))

(defun is-addr-definition-p (def)
   (not (is-worker-definition-p def)))

(defun definition-set-local-agg (def)
   (definition-add-option def :local-agg))
(defun definition-has-local-agg-p (def)
   (definition-has-option-p def :local-agg))
(defun definition-set-strata (def level)
   (definition-add-tagged-option def :strat level))
(defun definition-get-strata (def)
   (definition-get-tagged-option def :strat))

(defun is-init-p (def)
   (definition-has-option-p def :init-tuple))
(defun is-route-p (def)
   (definition-has-option-p def :route))
(defun is-reverse-route-p (def)
   (definition-has-tagged-option-p def :reverse-route))
(defun find-init-predicate (defs) (find-if #'is-init-p defs))
(defun find-init-predicate-name (defs)
   (definition-name (find-init-predicate defs)))
(defun get-routes (&optional (code *ast*))
   (filter #'is-route-p (definitions code)))
   
(defun get-route-names (&optional (code *ast*))
   (mapcar #'definition-name (get-routes code)))
   
(defun subgoal-matches-def-p (sub def)
   (equal (subgoal-name sub) (definition-name def)))
(defun subgoal-match-p (sub1 sub2)
   (equal (subgoal-name sub1) (subgoal-name sub2)))

(defun make-aggregate (agg typ mod) `(:aggregate ,agg ,typ ,mod))
(defun aggregate-agg (agg) (second agg))
(defun aggregate-type (agg) (third agg))

(defun aggregate-mod (agg) (fourth agg))
(defun aggregate-mod-is-input-p (aggmod) (tagged-p aggmod :input))
(defun aggregate-mod-is-output-p (aggmod) (tagged-p aggmod :output))
(defun aggregate-mod-is-immediate-p (aggmod) (eq aggmod :immediate))
(defun aggregate-mod-io-name (aggmod) (second aggmod))
(defun aggregate-mod-includes-home-p (aggmod)
   (and (> (length aggmod) 2)
      (eq (third aggmod) :home)))
(defun aggregate-mod-include-home (aggmod)
   (assert (= (length aggmod) 2))
   (push-end :home aggmod))

(defun definition-aggregate (def)
   (with-definition def (:types typs) (find-if #'aggregate-p typs)))

(defun arg-type (arg)
   (if (aggregate-p arg)
       (aggregate-type arg)
       arg))
(defun definition-arg-types (typs) (mapcar #'arg-type typs))

(defun definition-aggregate-p (def)
   (with-definition def (:types typs)
      (some #'aggregate-p typs)))

(defun make-extern (name ret-type types) `(:extern ,name ,ret-type ,types))
(defun extern-name (ext) (second ext))
(defun extern-ret-type (ext) (third ext))
(defun extern-types (ext) (fourth ext))

(defun make-constraint (expr &optional (priority 0)) (list :constraint expr priority))
(defun constraint-expr (ls) (second ls))
(defun constraint-priority (ls) (third ls))

(defun set-constraint-expr (constraint new-expr)
   (setf (second constraint) new-expr))
(defsetf constraint-expr set-constraint-expr)

(defmacro define-is-p (&rest symbs)
   `(on-top-level
      ,@(mapcar #'(lambda (sy)
            `(defun ,(intern (concatenate 'string (symbol-name sy) "-P")) (val)
                  (tagged-p val ,sy)))
         symbs)))
         
(define-is-p :int :float :var :plus :minus :mul :div :mod
            :equal :not-equal
            :lesser :lesser-equal :greater :greater-equal
            :convert-float :world :colocated
            :constraint :extern :aggregate
            :true :false :not :head
            :tail :cons :call :test-nil :addr
            :nil :host-id)

(defun const-p (s)
   (or (int-p s) (float-p s) (call-p s)
      (cons-p s) (nil-p s) (addr-p s)))
            
(defun make-op (op op1 op2)
   `(,op ,op1 ,op2))
(defun op-op (val) (tagged-tag val))
(defun op-op1 (val) (second val))
(defun op-op2 (val) (third val))

(defun set-op-op1 (o expr)
   (setf (second o) expr))
(defsetf op-op1 set-op-op1)

(defun set-op-op2 (o expr)
   (setf (third o) expr))
(defsetf op-op2 set-op-op2)

(defun op-p (val)
   (any (plus-p minus-p mul-p div-p mod-p not-equal-p equal-p lesser-p lesser-equal-p greater-p greater-equal-p) val))

(defun int-val (val) (second val))
(defun make-int (int &optional typ)
   (if typ
      `(:int ,int ,typ)
      `(:int ,int)))
(defun make-forced-int (int) (make-int int :type-int))

(defun float-val (val) (second val))
(defun make-float (flt) `(:float ,flt :type-float))

(defun make-host-id () '(:host-id :type-addr))

(defun make-convert-float (expr) `(:convert-float ,expr))
(defun convert-float-expr (flt) (second flt))

(defun set-convert-float-expr (c expr)
   (setf (second c) expr))
(defsetf convert-float-expr set-convert-float-expr)

(defun make-world () (list :world))

(defun var-name (val) (second val))
(defun var-eq-p (v1 v2) (equal (var-name v1) (var-name v2)))

;;;; ASSIGNMENTS

(defun make-assignment (var expr) (list :assign var expr))
(defun assignment-p (ls) (tagged-p ls :assign))
(defun assignment-var (ls) (second ls))
(defun assignment-expr (ls) (third ls))

(defun set-assignment-var (ass new-var)
   (setf (second ass) new-var))
(defsetf assignment-var set-assignment-var)

(defun set-assignment-expr (ass new-expr)
   (setf (third ass) new-expr))
(defsetf assignment-expr set-assignment-expr)

;;;; SUBGOALS

(defun subgoal-p (ls) (tagged-p ls :subgoal))
(defun subgoal-name (subgoal) (second subgoal))
(defun subgoal-args (subgoal) (third subgoal))
(defun set-subgoal-args (subgoal new-args)
   (setf (third subgoal) new-args))
(defsetf subgoal-args set-subgoal-args)

(defun lookup-definition-types (pred)
   (when-let ((def (lookup-definition pred)))
      (definition-types def)))

(defun lookup-definition (pred &optional (defs *definitions*))
   (find-if #L(string-equal pred (definition-name !1)) defs))

(defun lookup-extern (name)
   (find-if #L(string-equal name (extern-name !1)) *externs*))

(defun has-constraints-p (subgoals) (some #'constraint-p subgoals))
(defun has-assignments-p (subgoals) (some #'assignment-p subgoals))
   
(defun op-to-string (op)
   (case op
      (:plus "+")
      (:minus "-")
      (:mul "*")
      (:div "/")
      (:mod "%")
      (:equal "=")
      (:not-equal "!=")
      (:lesser "<")
      (:lesser-equal "<=")
      (:greater ">")
      (:greater-equal ">=")))
      
(defmacro eq-or (sym &rest symbols)
   `(or ,@(mapcar #'(lambda (s) `(eq ,sym ,s)) symbols)))
   
(defun eq-arith-p (sym) (eq-or sym :plus :minus :mul :div :mod))
(defun eq-cmp-p (sym) (eq-or sym :equal :not-equal :lesser :lesser-equal :greater :greater-equal))