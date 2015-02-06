(in-package :cl-meld)

(defparameter *tab-level* 0)
(defmacro format-code (stream str &rest rest)
   `(format ,stream "~{~a~}~a" (loop for i from 1 to *tab-level*
                                  collect "  ")
                     (tostring ,str ,@rest)))

(defmacro with-tab (&body body)
   `(let ((*tab-level* (1+ *tab-level*)))
      ,@body))

(defclass frame ()
   ((tuple
     :initarg :tuple
     :initform nil
     :accessor frame-tuple)
    (iterator
     :initarg :iterator
     :initform nil
     :accessor frame-iterator)
    (list
     :initarg :list
     :initform nil
     :accessor frame-list)
    (reg
     :initarg :reg
     :initform nil
     :accessor frame-reg)
    (definition
     :initarg :definition
     :initform nil
     :accessor frame-definition)
    (predicate
     :initarg :predicate
     :initform nil
     :accessor frame-predicate)
    (is-linear-p
     :initarg :is-linear-p
     :initform nil
     :accessor frame-is-linear-p)
    (start-loop
     :initarg :start-loop
     :initform nil
     :accessor frame-start-loop)))

(defmacro with-separate-c-context ((variables allocated-tuples) &body body)
   (alexandria:with-gensyms (old-var old-alloc)
      `(let ((,old-var ,variables)
            (,old-alloc ,allocated-tuples))
         (setf ,variables (copy-hash-table ,variables))
         (setf ,allocated-tuples (copy-hash-table ,allocated-tuples))
         ,@body
         (setf ,variables ,old-var)
         (setf ,allocated-tuples ,old-alloc))))

(defun create-c-type (stream typ)
	(cond
		((symbolp typ)
			(case typ
      		(:type-int (format stream "new type(FIELD_INT)"))
            (:type-float (format stream "new type(FIELD_FLOAT)"))
      		(:type-addr (format stream "new type(FIELD_NODE)"))
				(:type-bool (format stream "new type(FIELD_BOOL)"))
            (:type-thread (format stream "new type(FIELD_THREAD)"))
				(:type-string (format stream "new type(FIELD_STRING)"))
				(otherwise (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))
      ((type-node-p typ) (format stream "prog->add_type(new type(FIELD_NODE));"))
		((type-list-p typ)
         (format stream "new list_type(")
         (create-c-type stream (type-list-element typ))
         (format stream ")"))
      ((type-array-p typ)
         (format stream "new array_type(")
         (create-c-type stream (type-array-element typ))
         (format stream ")"))
		(t (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))

(defun locate-loop-frame (frames reg)
   (dolist (frame frames)
      (when (reg-eq-p (frame-reg frame) reg)
         (return-from locate-loop-frame frame)))
   nil)

(defun locate-first-loop-linear-frame (frames)
   (dolist (frame (reverse frames))
      (when (frame-is-linear-p frame)
         (return-from locate-first-loop-linear-frame frame))))

(defun locate-similar-tuples (frames def)
   (loop for frame in frames
         when (and (eq (frame-definition frame) def)
                   (frame-is-linear-p frame))
         collect (frame-tuple frame)))

(defun output-c-axiom-argument (stream arg id)
   (cond
    ((addr-p arg) (format-code stream "tpl->set_node(~a, (vm::node_val)All->DATABASE->find_node(~a));~%" id (vm-addr-num arg)))
    ((int-p arg)
      (if (type-float-p (expr-type arg))
         (format-code stream "tpl->set_float(~a, ~a);~%" id (int-val arg))
         (format-code stream "tpl->set_int(~a, ~a);~%" id (int-val arg))))
    ((float-p arg)
      (format-code stream "tpl->set_float(~a, ~a);~%" id (int-val arg)))
    ((nil-p arg)
      (format-code stream "tpl->set_cons(~a, nullptr);~%" id))
   (t (error 'output-invalid-error :text (tostring "output-c-axiom-argument: dont know")))))

(defun type-to-c-type (typ)
   (cond
     ((or (type-addr-p typ) (type-node-p typ)) "vm::node_val")
     ((type-int-p typ) "vm::int_val")
     ((type-float-p typ) "vm::float_val")
     ((type-list-p typ) "runtime::cons*")
     ((type-bool-p typ) "vm::bool_val")
     (t (error 'output-invalid-error :text (tostring "type-to-c-type: do not know ~a" typ)))))

(defun type-to-tuple-get (typ)
   (cond
     ((or (type-addr-p typ) (type-node-p typ)) "get_node")
     ((type-int-p typ) "get_int")
     ((type-float-p typ) "get_float")
     ((type-list-p typ) "get_cons")
     (t (error 'output-invalid-error :text (tostring "type-to-tuple-get: do not know ~a" typ)))))

(defun type-to-tuple-set (typ)
   (cond
     ((or (type-addr-p typ) (type-node-p typ)) "set_node")
     ((type-int-p typ) "set_int")
     ((type-float-p typ) "set_float")
     ((type-list-p typ) "set_cons")
     (t
      (error 'output-invalid-error :text (tostring "type-to-tuple-set: do not know ~a" typ)))))

(defun type-to-union-field (typ)
   (cond
     ((or (type-addr-p typ) (type-node-p typ)) (values "node_field" "vm::node_val"))
     ((type-int-p typ) (values "int_field" "vm::int_val"))
     ((type-float-p typ) (values "float_field" "vm::float_val"))
     ((type-list-p typ) (values "ptr_field" "vm::ptr_val"))
     ((type-bool-p typ) (values "bool_field" "vm::bool_val"))
     (t (error 'output-invalid-error :text (tostring "type-to-union-field: do not know ~a" typ)))))

(defun make-allocated-tuple (tpl pred def) (list tpl pred def))
(defun allocated-tuple-tpl (x) (first x))
(defun allocated-tuple-pred (x) (second x))
(defun allocated-tuple-definition (x) (third x))

(defun make-c-variable (typ name) (cons typ name))
(defun c-variable-type (x) (car x))
(defun c-variable-name (x) (cdr x))

(defun make-c-op (stream variables instr typ op)
 (let ((r1 (vm-op-v1 instr))
       (r2 (vm-op-v2 instr)))
  (multiple-value-bind (v1 found-p1) (gethash (reg-num r1) variables)
   (assert found-p1)
   (multiple-value-bind (v2 found-p2) (gethash (reg-num r2) variables)
    (assert found-p2)
    (let ((dest (generate-mangled-name "op"))
          (rdest (vm-op-dest instr)))
     (format-code stream "const ~a ~a(~a ~a ~a);~%" (type-to-c-type typ) dest (c-variable-name v1) op (c-variable-name v2))
     (setf (gethash (reg-num rdest) variables) (make-c-variable typ dest)))))))

(defun create-c-tuple-field-from-val (stream allocated-tuples variables typ value)
   (let ((name (generate-mangled-name "lookup")))
      (format-code stream "vm::tuple_field ~a;~%" name)
      (cond
       ((vm-int-p value) (format-code stream "~a.int_field = ~a;~%" name (vm-int-val value)))
       ((vm-float-p value) (format-code stream "~a.float_field = ~a;~%" name (vm-float-val value)))
       ((reg-dot-p value)
        (multiple-value-bind (tp found) (gethash (reg-num (reg-dot-reg value)) allocated-tuples)
          (multiple-value-bind (c-field c-cast) (type-to-union-field typ)
                (format-code stream "~a.~a = (~a)~a->~a(~a);~%" name c-field c-cast (allocated-tuple-tpl tp) (type-to-tuple-get typ) (reg-dot-field value)))))
       ((reg-p value)
        (multiple-value-bind (v found) (gethash (reg-num value) variables)
          (multiple-value-bind (c-field c-cast) (type-to-union-field typ)
            (format-code stream "~a.~a = (~a)~a;~%" name c-field c-cast (c-variable-name v)))))
       (t
        (error 'output-invalid-error :text (tostring "create-c-tuple-field-from-val: do not know how to handle ~a" value))))
      name))

(defun create-c-args (stream variables args)
   (loop for arg in args
         collect (let ((arg-name (generate-mangled-name "arg")))
                   (multiple-value-bind (v found) (gethash (reg-num arg) variables)
                     (assert found)
                     (format-code stream "vm::tuple_field ~a;~%" arg-name)
                     (multiple-value-bind (c-field c-cast) (type-to-union-field (c-variable-type v))
                        (format-code stream "~a.~a = (~a)~a;~%" arg-name c-field c-cast (c-variable-name v)))
                     arg-name))))

(defun create-c-call (stream call variables)
	(let* ((name (vm-call-name call))
          (extern-id (lookup-external-function-id name))
          (args (vm-call-args call))
          (rdest (vm-call-dest call))
          (dest (generate-mangled-name "call"))
          (gc-p (vm-call-gc call))
          (typ (vm-call-type call)))
    (format-code stream "const vm::tuple_field ~afield(vm::external::~a(~{~a~^, ~}));~%"  dest name (create-c-args stream variables args))
    (format-code stream "const ~a ~a(~afield.~a);~%" (type-to-c-type typ) dest dest (type-to-union-field typ))
    (setf (gethash (reg-num rdest) variables) (make-c-variable typ dest))))

(defun create-c-matches-code (stream tpl def matches allocated-tuples &optional (skip-code "continue;"))
   (loop for match in matches
         do (let* ((reg-dot (match-left match))
                   (field (reg-dot-field reg-dot))
                   (value (match-right match)))
               (cond
                ((vm-int-p value)
                 (let ((val (vm-int-val value)))
                  (format-code stream "if(~a->get_int(~a) != ~a) { ~a }~%" tpl field val skip-code)))
                ((reg-dot-p value)
                  (with-definition def (:types typs)
                     (let ((typ (nth field typs)))
                      (multiple-value-bind (other found) (gethash (reg-num (reg-dot-reg value)) allocated-tuples)
                       (assert found)
                        (format-code stream "if(~a->~a(~a) != ~a->~a(~a)) { ~a }~%"
                         tpl (type-to-tuple-get typ) field (allocated-tuple-tpl other) (type-to-tuple-get typ) (reg-dot-field value) skip-code)))))
                (t
                 (error 'output-invalid-error :text (tostring "create-c-matches: can't create code for value ~a" value)))))))

(defun iterate-match-constant-p (val)
   (cond
    ((vm-int-p val) t)
    ((vm-float-p val) t)
    ((reg-dot-p val) t)
    (t
     (error 'output-invalid-error :text (tostring "iterate-match-constant-p: do not know if value ~a should be a constant." val)))))

(defun iterate-matches-constant-at-p (matches index)
   (loop for match in matches
         do (let* ((reg-dot (match-left match))
                   (field (reg-dot-field reg-dot))
                   (value (match-right match)))
               (when (iterate-match-constant-p value)
                  (return-from iterate-matches-constant-at-p match))))
   nil)

(defun do-output-c-instr (stream instr frames allocated-tuples variables &key is-linear-p)
   (case (instr-type instr)
      (:return (format-code stream "return;~%"))
      (:next (format-code stream "continue;~%"))
      (:return-linear (format-code stream "break;~%"))
      (:rule-done (format-code stream "// rule done~%"))
      (:rule (format-code stream "// starting rule ~a~%" (vm-rule-id instr)))
      (:move-ptr-to-reg (format-code stream "// move ptr to reg not implemented.~%"))
      (:end-linear )
      (:return-select (format-code stream "break;~%"))
      (:convert-float
         (let ((place (vm-convert-float-place instr))
               (dest (vm-convert-float-dest instr))
               (name (generate-mangled-name "float")))
          (multiple-value-bind (v found) (gethash (reg-num place) variables)
            (format-code stream "const vm::float_val ~a((vm::float_val)~a);~%" name (c-variable-name v))
            (setf (gethash (reg-num dest) variables) (make-c-variable :type-float name)))))
      (:move-field-to-field
         (let ((from (move-from instr))
               (to (move-to instr)))
          (multiple-value-bind (ft found1) (gethash (reg-num (reg-dot-reg from)) allocated-tuples)
            (assert found1)
            (multiple-value-bind (tt found2) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
             (assert found2)
             (format-code stream "~a->set_field(~a, ~a->get_field(~a));~%"
               (allocated-tuple-tpl tt) (reg-dot-field to) (allocated-tuple-tpl ft) (reg-dot-field from))))))
      (:move-field-to-field-ref
         (let ((from (move-from instr))
               (to (move-to instr)))
          (multiple-value-bind (ft found1) (gethash (reg-num (reg-dot-reg from)) allocated-tuples)
            (assert found1)
            (multiple-value-bind (tt found2) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
             (assert found2)
             (format-code stream "~a->set_field_ref(~a, ~a->get_field(~a), ~a, state.gc_nodes);~%"
               (allocated-tuple-tpl tt) (reg-dot-field to) (allocated-tuple-tpl ft) (reg-dot-field from) (allocated-tuple-pred tt))))))
      (:cons-fff
         (let ((tail (vm-cons-tail instr))
               (head (vm-cons-head instr))
               (dest (vm-cons-dest instr)))
          (multiple-value-bind (tailt found1) (gethash (reg-num (reg-dot-reg tail)) allocated-tuples)
           (assert found1)
           (multiple-value-bind (headt found2) (gethash (reg-num (reg-dot-reg head)) allocated-tuples)
            (assert found2)
            (multiple-value-bind (destt found3) (gethash (reg-num (reg-dot-reg dest)) allocated-tuples)
             (assert found3)
             (format-code stream "~a->set_cons(~a, runtime::cons::create(~a->get_cons(~a), ~a->get_field(~a), (list_type*)~a->get_field_type(~a)));~%"
               (allocated-tuple-tpl destt) (reg-dot-field dest) (allocated-tuple-tpl tailt)
               (reg-dot-field tail) (allocated-tuple-tpl headt) (reg-dot-field head)
               (allocated-tuple-pred tailt) (reg-dot-field tail)))))))
      (:move-nil-to-field
         (let ((to (move-to instr)))
          (multiple-value-bind (tp found) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
           (assert found)
           (format-code stream "~a->~a(~a, nullptr);~%" (allocated-tuple-tpl tp) (type-to-tuple-set (make-list-type :all))
               (reg-dot-field to)))))
      (:move-host-id-to-reg
         (let ((to (move-to instr))
               (dest (generate-mangled-name "host")))
            (setf (gethash (reg-num to) variables) (make-c-variable :type-addr dest))
            (format-code stream "const vm::node_val ~a((vm::node_val)state.node);~%" dest)))
      (:move-host-id-to-field
         (let ((to (move-to instr))
               (dest (generate-mangled-name "host")))
            (multiple-value-bind (tp found) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
               (assert found)
               (format-code stream "~a->set_node(~a, (vm::node_val)state.node);~%" (allocated-tuple-tpl tp) (reg-dot-field to)))))
      (:move-float-to-reg
         (let ((from (move-from instr))
               (to (move-to instr))
               (dest (generate-mangled-name "floatconst")))
          (setf (gethash (reg-num to) variables) (make-c-variable :type-float dest))
          (format-code stream "const vm::float_val ~a(~a);~%" dest (vm-float-val from))))
      (:move-int-to-reg
         (let ((from (move-from instr))
               (to (move-to instr))
               (dest (generate-mangled-name "intconst")))
          (setf (gethash (reg-num to) variables) (make-c-variable :type-int dest))
          (format-code stream "const vm::int_val ~a(~a);~%" dest (vm-int-val from))))
      (:move-addr-to-reg
         (let ((from (move-from instr))
               (to (move-to instr))
               (dest (generate-mangled-name "nodeconst")))
          (setf (gethash (reg-num to) variables) (make-c-variable :type-addr dest))
          (format-code stream "static const vm::node_val ~a((vm::node_val)All->DATABASE->find_node(~a));~%" dest (vm-ptr-val from))))
      (:move-int-to-field
         (let ((from (move-from instr))
               (to (move-to instr)))
          (multiple-value-bind (tpl found) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
            (assert found)
             (format-code stream "~a->set_int(~a, ~a);~%" (allocated-tuple-tpl tpl) (reg-dot-field to) (vm-int-val from)))))
      (:call2
         (create-c-call stream instr variables))
      (:not
         (let ((place (vm-not-place instr))
               (dest (vm-not-dest instr))
               (val (generate-mangled-name "not")))
          (multiple-value-bind (v found) (gethash (reg-num place) variables)
           (assert found)
           (format-code stream "const vm::bool_val ~a(!~a);~%" val (c-variable-name v))
           (setf (gethash (reg-num dest) variables) (make-c-variable :type-bool val)))))
      (:addr-not-equal (make-c-op stream variables instr :type-bool "!="))
      (:int-greater (make-c-op stream variables instr :type-bool ">"))
      (:int-lesser (make-c-op stream variables instr :type-bool "<"))
      (:int-plus (make-c-op stream variables instr :type-int "+"))
      (:int-lesser-equal (make-c-op stream variables instr :type-bool "<="))
      (:int-equal (make-c-op stream variables instr :type-bool "=="))
      (:if-else
         (let ((r (vm-if-reg instr)))
            (multiple-value-bind (v found) (gethash (reg-num r) variables)
             (assert found)
             (format-code stream "if(~a) {~%" (c-variable-name v))
             (with-tab
               (with-separate-c-context (variables allocated-tuples)
                  (dolist (inner (vm-if-else-instrs1 instr))
                   (do-output-c-instr stream inner frames allocated-tuples variables :is-linear-p is-linear-p))))
             (format-code stream "} else {~%")
             (with-tab
               (with-separate-c-context (variables allocated-tuples)
                  (dolist (inner (vm-if-else-instrs2 instr))
                     (do-output-c-instr stream inner frames allocated-tuples variables :is-linear-p is-linear-p))))
             (format-code stream "}~%"))))
      (:if
         (let ((r (vm-if-reg instr))
               (instrs (vm-if-instrs instr)))
          (multiple-value-bind (v1 found-p) (gethash (reg-num r) variables)
            (format-code stream "if(~a) {~%" (c-variable-name v1))
            (with-tab
             (with-separate-c-context (variables allocated-tuples)
                (dolist (inner instrs)
                 (do-output-c-instr stream inner frames allocated-tuples variables :is-linear-p is-linear-p))))
            (format-code stream "}~%"))))
      (:move-reg-to-field
         (let* ((from (move-from instr))
                (to (move-to instr)))
          (multiple-value-bind (v found-p) (gethash (reg-num from) variables)
           (assert found-p)
           (let ((typ (c-variable-type v)))
            (multiple-value-bind (tp found2) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
               (assert found2)
               (format-code stream "~a->~a(~a, ~a);~%" (allocated-tuple-tpl tp)
                   (type-to-tuple-set typ) (reg-dot-field to) (c-variable-name v)))))))
      (:move-field-to-reg
         (let* ((from (move-from instr))
                (to (move-to instr))
                (dest (generate-mangled-name "val"))
                (frame (locate-loop-frame frames (reg-dot-reg from)))
                (def (frame-definition frame)))
          (with-definition def (:types types)
           (let* ((typ (nth (reg-dot-field from) types))
                  (c-type (type-to-c-type typ)))
            (format-code stream "const ~a ~a(~a->~a(~a));~%"
               c-type dest (frame-tuple frame) (type-to-tuple-get typ) (reg-dot-field from))
            (setf (gethash (reg-num to) variables) (make-c-variable typ dest))))))
      (:send (let* ((to (send-to instr))
                    (from (send-from instr)))
              (multiple-value-bind (p found-p) (gethash (reg-num to) variables)
                  (multiple-value-bind (tp found2) (gethash (reg-num from) allocated-tuples)
                    (format-code stream "state.sched->new_work(state.node, (db::node*)~a, ~a, ~a, state.direction, state.depth);~%"
                        (c-variable-name p) (allocated-tuple-tpl tp) (allocated-tuple-pred tp))))))
      (:new-axioms
         (let ((axioms (vm-new-axioms-subgoals instr)))
            (do-subgoals axioms (:name name :args args :subgoal axiom)
               (let ((id (lookup-def-id name))
                     (def (lookup-definition name)))
                  (format-code stream "{~%")
                  (with-tab
                     (format-code stream "// add ~a(~a)~%" name args)
                     (format-code stream "predicate *pred(prog->get_predicate(~a));~%" id)
                     (format-code stream "tuple *tpl(vm::tuple::create(pred));~%")
                     (loop for arg in args
                           for id from 0
                           do (output-c-axiom-argument stream arg id))
                     (cond
                      ((is-linear-p def)
                       (format-code stream "state.node->add_linear_fact(tpl, pred);~%"))
                      (t
                       (format-code stream "state.node->store.persistent_tuples.push_back(new full_tuple(tpl, pred, state.direction, state.depth));~%"))))
                  (format-code stream "}~%")))))
      (:select-node
          (when (vm-select-node-empty-p instr)
           (return-from do-output-c-instr nil))
          (format-code stream "switch(state.node->get_id()) {~%")
          (with-tab
             (vm-select-node-iterate instr (n instrs)
              (format-code stream "case ~a: {~%" n)
              (with-tab
                  (dolist (inner instrs)
                     (do-output-c-instr stream inner frames allocated-tuples variables :is-linear-p is-linear-p)))
                (format-code stream "}~%")))
          (format-code stream "}~%"))
      (:return-derived
         ;; locate first linear
         (let ((first-linear (if is-linear-p (locate-first-loop-linear-frame frames) nil)))
            (cond
             (first-linear
               (let ((it (frame-iterator first-linear))
                     (goto (frame-start-loop first-linear)))
                (format-code stream "goto ~a;~%" goto)))
             (t ))))
      (:alloc (let ((tuple-id (lookup-def-id (vm-alloc-tuple instr)))
                    (def (lookup-definition (vm-alloc-tuple instr)))
                    (reg (vm-alloc-reg instr))
                    (pred (generate-mangled-name "pred"))
                    (tpl (generate-mangled-name "tpl")))
                 (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl pred def))
                 (format-code stream "static predicate *~a(prog->get_predicate(~a));~%" pred tuple-id)
                 (format-code stream "tuple *~a(vm::tuple::create(~a));~%" tpl pred)))
      (:add-linear (let ((reg (vm-add-linear-reg instr)))
                    (multiple-value-bind (p found-p) (gethash (reg-num reg) allocated-tuples)
                        (format-code stream "state.node->add_linear_fact(~a, ~a);~%" (allocated-tuple-tpl p) (allocated-tuple-pred p)))))
      (:update
         (multiple-value-bind (tp found) (gethash (reg-num (vm-update-reg instr)) allocated-tuples)
            (format-code stream "// tuple ~a is updated now.~%" (allocated-tuple-tpl tp)))) ;; do nothing for now
      (:remove (let ((reg (vm-remove-reg instr)))
                (multiple-value-bind (p found-p) (gethash (reg-num reg) allocated-tuples)
                  (let* ((frame (locate-loop-frame frames reg))
                         (def (frame-definition frame))
                         (pred (frame-predicate frame)))
                     (when (is-reused-p def)
                        (format-code stream "if(state.direction == POSITIVE_DERIVATION) {~%")
                        (with-tab
                           (format-code stream "state.node->store.persistent_tuples.push_back(full_tuple::remove_new(~a, ~a, state.depth));~%"
                               (allocated-tuple-tpl p) pred))
                        (format-code stream "}~%"))
                     (let* ((ls (frame-list frame))
                            (tpl (frame-tuple frame))
                            (pred (frame-predicate frame))
                            (it (frame-iterator frame)))
                        (format-code stream "~a = ~a->erase(~a);~%" it ls it)
                        (format-code stream "vm::tuple::destroy(~a, ~a, state.gc_nodes);~%" tpl pred))))))
      (:rlinear-iterate (let* ((def (lookup-definition (iterate-name instr)))
                              (id (lookup-def-id (iterate-name instr)))
                              (lsname (generate-mangled-name "ls"))
                              (it (generate-mangled-name "it"))
                              (tpl (generate-mangled-name "tpl"))
                              (predicate (generate-mangled-name "pred"))
                              (reg (iterate-reg instr))
                              (frame (make-instance 'frame
                                      :list lsname
                                      :tuple tpl
                                      :iterator it
                                      :definition def
                                      :predicate predicate
                                      :reg reg
                                      :is-linear-p nil
                                      :start-loop nil)))
                           (flet ((create-inner-code (iterator)
                                   (with-tab
                                       (format-code stream "tuple *~a(*~a);~%" tpl iterator)
                                       (let ((similar-tpls (locate-similar-tuples frames def)))
                                          (dolist (sim similar-tpls)
                                             (format-code stream "if(~a == ~a) {~%" tpl sim)
                                             (with-tab
                                                (format-code stream "~a++;~%" iterator)
                                                (format-code stream "continue;~%"))
                                             (format-code stream "}~%")))
                                       (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl predicate def))
                                       (create-c-matches-code stream tpl def (iterate-matches instr) allocated-tuples (tostring "~a++; continue;" iterator))
                                       (dolist (inner (iterate-instrs instr))
                                          (do-output-c-instr stream inner (cons frame frames) allocated-tuples variables :is-linear-p is-linear-p))
                                       (format-code stream "~a++;~%" iterator)
                                       (setf (gethash (reg-num reg) allocated-tuples) nil))))
                              (with-definition def (:name name :types types)
                                 (format-code stream "// iterate through predicate ~a~%" (iterate-name instr))
                                 (format-code stream "static predicate *~a(prog->get_predicate(~a));~%" predicate id)
                                 (format-code stream "utils::intrusive_list<vm::tuple> *~a(state.node->linear.get_linked_list(~a));~%" lsname id)
                                 (format-code stream "for(auto ~a(~a->begin()), ~aend(~a->end()); ~a != ~aend; ) {~%" it lsname it lsname it it)
                                 (create-inner-code it)
                                 (format-code stream "}~%")))))
      (:linear-iterate (let* ((def (lookup-definition (iterate-name instr)))
                              (types (definition-types def))
                              (id (lookup-def-id (iterate-name instr)))
                              (lsname (generate-mangled-name "ls"))
                              (it (generate-mangled-name "it"))
                              (needs-label-p (not (locate-first-loop-linear-frame frames)))
                              (tpl (generate-mangled-name "tpl"))
                              (start-loop (if needs-label-p (generate-mangled-name "loop")))
                              (predicate (generate-mangled-name "pred"))
                              (reg (iterate-reg instr))
                              (index (find-index-name (iterate-name instr)))
                              (frame (make-instance 'frame
                                      :list lsname
                                      :tuple tpl
                                      :iterator it
                                      :definition def
                                      :predicate predicate
                                      :reg reg
                                      :is-linear-p t
                                      :start-loop start-loop)))
                        (flet ((create-inner-code (iterator)
                                 (format-code stream "tuple *~a(*~a);~%" tpl iterator)
                                 (let ((similar-tpls (locate-similar-tuples frames def)))
                                    (dolist (sim similar-tpls)
                                       (format-code stream "if(~a == ~a) {~%" tpl sim)
                                       (with-tab
                                          (format-code stream "~a++;~%" iterator)
                                          (format-code stream "continue;~%"))
                                       (format-code stream "}~%")))
                                 (create-c-matches-code stream tpl def (iterate-matches instr) allocated-tuples (tostring "~a++; continue;" iterator))
                                 (format-code stream "{~%")
                                 (with-tab
                                    (dolist (inner (iterate-instrs instr))
                                       (do-output-c-instr stream inner (cons frame frames) allocated-tuples variables :is-linear-p t)))
                                 (format-code stream "}~%")))
                        (with-definition def (:name name :types types)
                           (format-code stream "// iterate predicate ~a~%" (iterate-name instr))
                           (format-code stream "static predicate *~a(prog->get_predicate(~a));~%" predicate id)
                           (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl predicate def))
                           (cond
                            ((null index)
                              (format-code stream "auto *~a(state.node->linear.get_linked_list(~a));~%" lsname id)
                              (format-code stream "for(auto ~a(~a->begin()), ~aend(~a->end()); ~a != ~aend; ) {~%" it lsname it lsname it it)
                              (with-tab (create-inner-code it))
                              (format-code stream "~a++;~%" it)
                              (when needs-label-p
                                 (format-code stream "~a:continue;~%" start-loop))
                              (format-code stream "}~%"))
                            (t
                              (format-code stream "if(state.node->linear.stored_as_hash_table(~a)) {~%" predicate)
                              (with-tab
                               (let ((table (generate-mangled-name "table")))
                                (format-code stream "hash_table *~a(state.node->linear.get_hash_table(~a));~%" table id)
                                (format-code stream "if(~a != nullptr) {~%" table)
                                (with-tab
                                  (let ((match (iterate-matches-constant-at-p (iterate-matches instr) (index-field index))))
                                   (cond
                                    (match
                                       (let* ((value (match-right match))
                                              (match-field (reg-dot-field (match-left match)))
                                              (tuple-field (create-c-tuple-field-from-val stream allocated-tuples variables (nth match-field types) value)))
                                        (format-code stream "// search hash table for ~a~%" tuple-field)
                                        (format-code stream "utils::intrusive_list<vm::tuple> *~a(~a->lookup_list(~a));~%"
                                             lsname table tuple-field)
                                        (format-code stream "for(auto ~a(~a->begin()), ~aend(~a->end()); ~a != ~aend; ) {~%" it lsname it lsname it it)
                                        (with-tab (create-inner-code it))
                                        (format-code stream "~a++;~%" it)
                                        (when needs-label-p
                                          (format-code stream "~a:continue;~%" start-loop))
                                        (format-code stream "}~%")))
                                    (t
                                       (let ((it2 (generate-mangled-name "it")))
                                        (format-code stream "// go through hash table~%")
                                        (format-code stream "for(hash_table::iterator ~a(~a->begin()); !~a.end(); ++~a) {~%" it2 table it2 it2)
                                        (with-tab
                                           (format-code stream "utils::intrusive_list<vm::tuple> *~a(*~a);~%" lsname it2)
                                           (format-code stream "for(auto ~a(~a->begin()), ~aend(~a->end()); ~a != ~aend; ) {~%" it lsname it lsname it it)
                                           (with-tab (create-inner-code it))
                                           (format-code stream "~a++;~%" it)
                                           (when needs-label-p
                                             (format-code stream "~a:continue;~%" start-loop))
                                           (format-code stream "}~%"))
                                        (format-code stream "}~%"))))))
                                 (format-code stream "}~%")))
                              (format-code stream "} else {~%")
                              (with-tab
                                 (format-code stream "auto *~a(state.node->linear.get_linked_list(~a));~%" lsname id)
                                 (format-code stream "for(auto ~a(~a->begin()), ~aend(~a->end()); ~a != ~aend; ) {~%" it lsname it lsname it it)
                                 (with-tab (create-inner-code it))
                                 (format-code stream "~a++;~%" it)
                                 (when needs-label-p
                                    (format-code stream "~a:continue;~%" start-loop))
                                 (format-code stream "}~%"))
                              (format-code stream "}~%"))))
                           (setf (gethash (reg-num reg) allocated-tuples) nil))))
      (:persistent-iterate (let* ((def (lookup-definition (iterate-name instr)))
                                  (id (lookup-def-id (iterate-name instr)))
                                  (it (generate-mangled-name "it"))
                                  (tpl (generate-mangled-name "tpl"))
                                  (predicate (generate-mangled-name "pred"))
                                  (reg (iterate-reg instr))
                                  (frame (make-instance 'frame
                                          :list nil
                                          :tuple tpl
                                          :iterator it
                                          :definition def
                                          :predicate predicate
                                          :reg reg
                                          :is-linear-p t
                                          :start-loop nil)))
                        (with-definition def (:name name :types types)
                           (format-code stream "// iterate through predicate ~a~%" (iterate-name instr))
                           (format-code stream "static predicate *~a(prog->get_predicate(~a));~%" predicate id)
                           (format-code stream "db::tuple_trie::tuple_search_iterator ~a(state.node->pers_store.match_predicate(~a->get_id(), nullptr));~%" it predicate)
                           (format-code stream "for(auto ~aend(tuple_trie::match_end()); ~a != ~aend; ++~a) {~%" it it it it)
                           (with-tab
                              (format-code stream "tuple_trie_leaf *~aleaf(*~a);~%" tpl it)
                              (format-code stream "tuple *~a(~aleaf->get_underlying_tuple());~%" tpl tpl)
                              (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl predicate def))
                              (create-c-matches-code stream tpl def (iterate-matches instr) allocated-tuples)
                              (dolist (inner (iterate-instrs instr))
                                 (do-output-c-instr stream inner (cons frame frames) allocated-tuples variables :is-linear-p is-linear-p))
                              (setf (gethash (reg-num reg) allocated-tuples) nil)))
                           (format-code stream "}~%")))
      (:reset-linear (dolist (inner (vm-reset-linear-instrs instr))
                        (do-output-c-instr stream inner frames allocated-tuples variables :is-linear-p nil)))
      (:set-priority-here
         (let ((r (vm-set-priority-priority instr)))
            (multiple-value-bind (v found) (gethash (reg-num r) variables)
               (assert found)
               (format-code stream "if(scheduling_mechanism) {~%")
               (with-tab
                  (format-code stream "state.sched->set_node_priority(state.node, ~a);~%" (c-variable-name v)))
               (format-code stream "}~%"))))
      (:set-priority
         (let ((prio (vm-set-priority-priority instr))
               (node (vm-set-priority-node instr)))
          (multiple-value-bind (vprio found) (gethash (reg-num prio) variables)
            (assert found)
            (multiple-value-bind (vnode found) (gethash (reg-num node) variables)
             (assert found)
             (format-code stream "if(scheduling_mechanism) {~%")
             (with-tab
              (format-code stream "state.sched->set_node_priority((db::node*)~a, ~a);~%" (c-variable-name vnode) (c-variable-name vprio)))
             (format-code stream "}~%")))))
      (otherwise (warn "not implemented ~a" instr))))

(defun do-output-c-header (stream)
   (setf *name-counter* 0)
   (format-code stream "#define POOL_ALLOCATOR~%")
   (format-code stream "#define USE_REAL_NODES~%")
   (format-code stream "#define USE_SPINLOCK~%")
   (format-code stream "#include \"interface.hpp\"~%")
   (format-code stream "#include \"external/others.hpp\"~%")
   (format-code stream "#include \"db/database.hpp\"~%")
   (format-code stream "#include \"db/node.hpp\"~%")
   (format-code stream "#include \"vm/program.hpp\"~%")
   (format-code stream "#include \"vm/state.hpp\"~%")
   (format-code stream "#include \"vm/tuple.hpp\"~%")
   (format-code stream "#include \"thread/threads.hpp\"~%")
   (format-code stream "~%")
   (format-code stream "using namespace vm;~%")
   (format-code stream "using namespace db;~%")
   (format-code stream "using namespace utils;~%")
   (format-code stream "~%")
   (format-code stream "void add_definitions(vm::program *prog) {~%")
   (with-tab
      (loop for typ in *program-types*
            do (progn
                  (format-code stream "prog->add_type(")
                  (create-c-type stream typ)
                  (format streaM ");~%")))
      (format-code stream "~%")
      (format-code stream "prog->num_predicates_uint = next_multiple_of_uint(~a);~%" (length *definitions*))
      (format-code stream "prog->number_rules = ~a;~%" (length *code-rules*))
      (format-code stream "prog->number_rules_uint = next_multiple_of_uint(prog->num_rules());~%")
      (do-definitions (:definition def :name name :types types :id id)
         (format-code stream "{~%")
         (with-tab
            (format-code stream "predicate *p(new predicate());~%")
            (format-code stream "p->id = ~a;~%" id)
            (format-code stream "p->is_linear = ~a;~%" (if (is-linear-p def) "true" "false"))
            (format-code stream "p->is_reverse_route = ~a;~%" (if (is-reverse-route-p def) "true" "false"))
            (format-code stream "p->is_action = ~a;~%" (if (is-action-p def) "true" "false"))
            (format-code stream "p->is_reused = ~a;~%" (if (is-reused-p def) "true" "false"))
            (format-code stream "p->is_thread = ~a;~%" (if (definition-is-thread-p def) "true" "false"))
            (format-code stream "p->types.resize(~a);~%" (length types))
            (loop for typ in types
                  for i from 0
                  do (format-code stream "p->types[~a] = prog->get_type(~a);~%" i (lookup-type-id typ)))
            (format-code stream "p->name = \"~a\";~%" name)
            (format-code stream "p->cache_info(prog);~%")
            (let ((index (find-index-name name)))
               (when index
                (format-code stream "p->store_as_hash_table(~a);~%" (- (index-field index) 2))))
            (format-code stream "prog->add_predicate(p);~%"))
         (format-code stream "}~%"))
      (format-code stream "prog->sort_predicates();~%")
      (loop for code-rule in *code-rules*
            for count from 0
            do (let ((str (rule-string code-rule)))
                  (format-code stream "{~%")
                  (with-tab
                     (format-code stream "rule *r(new rule(~a, std::string(\"~a\")));~%" count str)
                     (format-code stream "prog->rules.push_back(r);~%")
                     (loop for id in (subgoal-ids code-rule)
                           do (format-code stream "r->add_predicate(~a);~%" id)
                           do (format-code stream "prog->predicates[~a]->add_linear_affected_rule(r);~%" id)))
                  (format-code stream "}~%"))))
   (format-code stream "}~%~%")
	(loop for code-rule in *code-rules*
			for count = 0 then (1+ count)
         for code = (rule-code code-rule)
			do (format-code stream "static inline void perform_rule~a(state& state, vm::program *prog) {~%" count)
         do (setf *name-counter* 0)
         do (with-tab
               (let ((allocated-tuples (make-hash-table))
                     (variables (make-hash-table)))
                  (dolist (instr code)
                     (do-output-c-instr stream instr nil allocated-tuples variables :is-linear-p nil))))
         do (format-code stream "}~%~%"))
   (format-code stream "void run_rule(state *s, program *prog, const size_t rule) {~%")
   (with-tab
      (format-code stream "switch(rule) {~%")
      (with-tab
         (loop for rule in *code-rules*
               for count from 0
               do (format-code stream "case ~a: perform_rule~a(*s, prog); break;~%" count count))
         (format-code stream "default: abort(); break;~%"))
      (format-code stream "}~%"))
   (format-code stream "}~%"))

(defun do-output-c-code (stream)
   (do-output-c-header stream))
	;(write-hexa stream (args-needed *ast*))
   ;(write-rules stream)
   ;(let* ((*output-string-constants* nil)
	;		 (processes (output-processes))
   ;       (descriptors (output-descriptors))
	;		 (rules (output-all-rules))
	;		 (consts (output-consts))
	;		 (functions (output-functions)))
		;; output strings
	;	(do-output-string-constants stream)
		; output constant code
	;	(do-output-consts stream consts)
		; write functions
	;	(do-output-functions stream functions)
		; write external definitions
	;	(do-output-externs stream)
		; output predicate descriptions
    ;  (do-output-descriptors stream descriptors processes)
		; output global priority predicate, if any
	;	(output-initial-priority stream)
   ;   (dolist (bc processes)
	;		(write-byte-code stream bc))
	;	(do-output-rules stream rules)))

(defun output-c-code (file &key (write-ast nil) (write-code nil))
   (let ((c-file (concatenate 'string file ".cpp")))
      (with-output-file (stream c-file)
         (do-output-c-code stream))))
