(in-package :cl-meld)

(defparameter *facts-generated* nil)

(defparameter *tab-level* 0)
(defun current-c-tab ()
   (tostring "~{~a~}" (loop for i from 1 to *tab-level* collect "  ")))
(defmacro format-code (stream str &rest rest)
   `(format ,stream "~a~a" (current-c-tab) (tostring ,str ,@rest)))

(defmacro with-tab (&body body)
   `(let ((*tab-level* (1+ *tab-level*)))
      ,@body))

(defmacro with-debug (stream macro &body body)
 `(progn
    (format ,stream (tostring "#ifdef ~a~%" ,macro))
    ,@body
    (format ,stream "#endif~%")))

(defun create-variable-context () (make-hash-table :test #'equal))
(defun create-allocated-tuples-context () (make-hash-table :test #'equal))
(defun good-c-name (name) (replace-all name "-" "_"))

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
    (hash
     :initarg :hash
     :initform nil
     :accessor frame-hash)
    (ordered
     :initarg :ordered
     :initform nil
     :accessor frame-ordered-vector)
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
				(otherwise (error 'output-invalid-error :text (tostring "create-c-type: invalid arg type: ~a" typ)))))
      ((type-node-p typ) (format stream "new type(FIELD_NODE);"))
		((type-list-p typ)
         (format stream (tostring "new list_type(type_~a)" (lookup-type-id (type-list-element typ)))))
      ((type-array-p typ)
         (format stream (tostring "new array_type(type_~a)" (lookup-type-id (type-array-element typ)))))
      ((type-struct-p typ)
         (format stream "new struct_type({~{~a~^, ~}})"
            (loop for typ in (type-struct-list typ)
                  collect (tostring "type_~a" (lookup-type-id typ)))))
		(t (error 'output-invalid-error :text (tostring "create-c-type: invalid arg type: ~a" typ)))))

(defun locate-loop-frame (frames reg)
   (dolist (frame frames)
      (when (reg-eq-p (frame-reg frame) reg)
         (return-from locate-loop-frame frame)))
   nil)

(defun locate-first-loop-linear-frame (frames)
   (dolist (frame frames)
      (when (and (frame-is-linear-p frame) (frame-start-loop frame))
         (return-from locate-first-loop-linear-frame frame))))

(defun locate-similar-tuples (frames def)
   (loop for frame in frames
         when (eq (frame-definition frame) def)
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
     ((type-array-p typ) "runtime::array*")
     ((type-struct-p typ) "runtime::struct1*")
     ((type-string-p typ) "runtime::rstring*")
     (t (error 'output-invalid-error :text (tostring "type-to-c-type: do not know ~a" typ)))))

(defun type-to-tuple-get (typ)
   (cond
     ((or (type-addr-p typ) (type-node-p typ)) "get_node")
     ((type-int-p typ) "get_int")
     ((type-float-p typ) "get_float")
     ((type-list-p typ) "get_cons")
     ((type-array-p typ) "get_array")
     ((type-struct-p typ) "get_struct")
     (t (error 'output-invalid-error :text (tostring "type-to-tuple-get: do not know ~a" typ)))))

(defun type-to-tuple-set (typ)
   (cond
     ((or (type-addr-p typ) (type-node-p typ)) "set_node")
     ((type-int-p typ) "set_int")
     ((type-float-p typ) "set_float")
     ((type-list-p typ) "set_cons")
     ((type-array-p typ) "set_array")
     ((type-struct-p typ) "set_struct")
     (t
      (error 'output-invalid-error :text (tostring "type-to-tuple-set: do not know ~a" typ)))))

(defun type-to-union-field (typ)
   (cond
     ((or (type-addr-p typ) (type-node-p typ)) (values "node_field" "vm::node_val"))
     ((type-int-p typ) (values "int_field" "vm::int_val"))
     ((type-float-p typ) (values "float_field" "vm::float_val"))
     ((type-list-p typ) (values "ptr_field" "vm::ptr_val"))
     ((type-array-p typ) (values "ptr_field" "vm::ptr_val"))
     ((type-struct-p typ) (values "ptr_field" "vm::ptr_val"))
     ((type-bool-p typ) (values "bool_field" "vm::bool_val"))
     ((type-string-p typ) (values "ptr_field" "vm::ptr_val"))
     (t (error 'output-invalid-error :text (tostring "type-to-union-field: do not know ~a" typ)))))

(defun make-allocated-tuple (tpl pred def) (list tpl pred def))
(defun allocated-tuple-tpl (x) (first x))
(defun allocated-tuple-pred (x) (second x))
(defun allocated-tuple-definition (x) (third x))

(defun make-c-variable (typ name reg) `(,typ ,name ,reg))
(defun c-variable-type (x) (first x))
(defun c-variable-name (x) (second x))
(defun c-variable-reg (x) (third x))

(defun allocate-c-variable (variables reg typ)
   "If variable is already registered in variables with the same type, then just get the same variable."
   (assert (or (vm-stack-p reg) (eq reg 'stack) (reg-p reg)))
   (multiple-value-bind (var found-p) (gethash (if (reg-p reg) (reg-num reg) reg) variables)
      (cond
       (found-p
        (let ((old-typ (c-variable-type var)))
            (if (simple-type-eq-p typ old-typ)
               (values var nil)
               (values (make-c-variable typ (generate-mangled-name "var") reg) t))))
       (t
        (values (make-c-variable typ (generate-mangled-name "var") reg) t)))))

(defun declare-c-variable (var new-p)
   (if new-p
      (tostring "~a ~a" (type-to-c-type (c-variable-type var)) (c-variable-name var))
      (c-variable-name var)))

(defun find-c-variable (variables reg)
   (multiple-value-bind (var found-p) (gethash (reg-num reg) variables)
      (assert found-p)
      var))

(defun add-c-variable (variables var)
   (let ((reg (c-variable-reg var)))
      (setf (gethash (if (reg-p reg) (reg-num reg) reg) variables) var)))

(defun make-c-op (stream variables instr typ op)
 (let ((r1 (vm-op-v1 instr))
       (r2 (vm-op-v2 instr))
       (rdest (vm-op-dest instr)))
  (multiple-value-bind (v1 found-p1) (gethash (reg-num r1) variables)
   (assert found-p1)
   (multiple-value-bind (v2 found-p2) (gethash (reg-num r2) variables)
    (assert found-p2)
    (multiple-value-bind (var new-p) (allocate-c-variable variables rdest typ)
     (format-code stream "~a = ~a ~a ~a;~%" (declare-c-variable var new-p) (c-variable-name v1) op (c-variable-name v2))
     (add-c-variable variables var))))))

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
       ((vm-stack-p value)
        (multiple-value-bind (v found) (gethash value variables)
         (assert found)
            (multiple-value-bind (c-field c-cast) (type-to-union-field typ)
               (format-code stream "~a.~a = (~a)~a;~%" name c-field c-cast (c-variable-name v)))))
       ((reg-p value)
        (multiple-value-bind (v found) (gethash (reg-num value) variables)
          (multiple-value-bind (c-field c-cast) (type-to-union-field typ)
            (format-code stream "~a.~a = (~a)~a;~%" name c-field c-cast (c-variable-name v)))))
       (t
        (error 'output-invalid-error :text (tostring "create-c-tuple-field-from-val: do not know how to handle ~a" value))))
      name))

(defun type-to-gc-function (typ)
   (cond
    ((type-list-p typ) "add_cons")
    ((type-struct-p typ) "add_struct")
    ((type-array-p typ) "add_array")
    (t
     (error 'output-invalid-error :text (tostring "type-to-gc-function: do not know how to handle ~a" typ)))))

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
          (gc-p (vm-call-gc call))
          (typ (vm-call-type call)))
    (multiple-value-bind (var new-p) (allocate-c-variable variables rdest typ)
      (let ((tmp (generate-mangled-name "tmp")))
       (format-code stream "const vm::tuple_field ~a(vm::external::~a(~{~a~^, ~}));~%" tmp name (create-c-args stream variables args))
       (format-code stream "~a = (~a)~a.~a;~%" (declare-c-variable var new-p) (type-to-c-type typ) tmp (type-to-union-field typ))
       (when (and (vm-bool-val gc-p) (reference-type-p typ))
         (format-code stream "if(~a) {~%" (c-variable-name var))
         (with-tab
            (format-code stream "state.~a((~a)~a);~%" (type-to-gc-function typ) (type-to-c-type typ) (c-variable-name var)))
         (format-code stream "}~%"))
       (add-c-variable variables var)))))

(defun create-c-matches-code (stream tpl def matches allocated-tuples &optional (skip-code "continue;") (match-field nil))
   (loop for match in matches
         do (let* ((reg-dot (match-left match))
                   (field (reg-dot-field reg-dot))
                   (value (match-right match))
                   (does-not-match-p (or (not match-field) (not (= field match-field)))))
                (cond
                 ((vm-int-p value)
                  (when does-not-match-p
                     (let ((val (vm-int-val value)))
                      (format-code stream "if(~a->get_int(~a) != ~a) { ~a }~%" tpl field val skip-code))))
                 ((vm-float-p value)
                  (when does-not-match-p
                     (let ((val (vm-float-val value)))
                      (format-code stream "if(~a->get_float(~a) != ~a) { ~a }~%" tpl field val skip-code))))
                 ((vm-nil-p value)
                  (when does-not-match-p
                     (format-code stream "if(!runtime::cons::is_null(~a->get_cons(~a))) { ~a }~%" tpl field skip-code)))
                 ((reg-dot-p value)
                  (with-definition def (:types typs)
                   (let ((typ (nth field typs)))
                    (multiple-value-bind (other found) (gethash (reg-num (reg-dot-reg value)) allocated-tuples)
                     (assert found)
                     (format-code stream "if(~a->~a(~a) != ~a->~a(~a)) { ~a }~%"
                      tpl (type-to-tuple-get typ) field (allocated-tuple-tpl other) (type-to-tuple-get typ) (reg-dot-field value) skip-code)))))
                 ((vm-non-nil-p value)
                  (format-code stream "if(runtime::cons::is_null(~a->get_cons(~a))) { ~a }~%" tpl field skip-code))
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
               (when (and (= index field) (iterate-match-constant-p value))
                  (return-from iterate-matches-constant-at-p match))))
   nil)

(defun compile-c-persistent-iterate (stream instr frames variables allocated-tuples node &key is-linear-p)
 (let* ((def (lookup-definition (iterate-name instr)))
        (id (lookup-def-id (iterate-name instr)))
        (it (generate-mangled-name "it"))
        (tpl (generate-mangled-name "tpl"))
        (predicate (tostring "pred_~a" id))
        (reg (iterate-reg instr))
        (frame (make-instance 'frame
                :list nil
                :tuple tpl
                :iterator it
                :definition def
                :predicate predicate
                :reg reg
                :is-linear-p nil
                :start-loop nil)))
  (with-definition def (:name name :types types)
   (format-code stream "// iterate through predicate ~a~%" (iterate-name instr))
   (format-code stream "db::tuple_trie::tuple_search_iterator ~a(~a->pers_store.match_predicate(~a->get_id(), nullptr));~%" it node predicate)
   (format-code stream "for(auto ~aend(tuple_trie::match_end()); ~a != ~aend; ++~a) {~%" it it it it)
   (with-tab
    (with-separate-c-context (variables allocated-tuples)
     (format-code stream "tuple_trie_leaf *~aleaf(*~a);~%" tpl it)
     (format-code stream "tuple *~a(~aleaf->get_underlying_tuple());~%" tpl tpl)
     (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl predicate def))
     (create-c-matches-code stream tpl def (iterate-matches instr) allocated-tuples)
     (with-debug stream "DEBUG_ITER"
      (format-code stream "std::cout << \"\\titerate \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%" tpl predicate))
     (dolist (inner (iterate-instrs instr))
      (do-output-c-instr stream inner (cons frame frames) allocated-tuples variables :is-linear-p is-linear-p)))))
  (format-code stream "}~%")))

(defun c-iterate-order-facts (stream instr predicate vec)
 (let ((sub (order-iterate-subgoal instr)))
  (cond
   ((subgoal-has-random-p sub)
    (format-code stream "utils::shuffle_vector(~a, state.randgen);~%" vec))
   ((subgoal-has-min-p sub)
    (format-code stream "sort(~a.begin(), ~a.end(), tuple_sorter(~a, ~a));~%" vec vec  (subgoal-get-min-variable-position sub) predicate))
   (t (assert nil)))))

(defun c-iterate-add-vector (stream tpl lsname it vec)
   (format-code stream "iter_object obj;~%")
   (format-code stream "obj.tpl = ~a;~%" tpl)
   (format-code stream "obj.iterator = ~a;~%" it)
   (format-code stream "obj.ls = ~a;~%" lsname)
   (format-code stream "~a.push_back(obj);~%" vec))

(defun compile-c-order-linear-iterate (stream instr frames variables allocated-tuples node &key is-linear-p has-removes-p)
   (let* ((def (lookup-definition (iterate-name instr)))
          (types (definition-types def))
          (id (lookup-def-id (iterate-name instr)))
          (lsname (generate-mangled-name "ls"))
          (it (generate-mangled-name "it"))
          (vec (generate-mangled-name "vec"))
          (needs-label-p (and has-removes-p (not is-linear-p)))
          (tpl (generate-mangled-name "tpl"))
          (predicate (tostring "pred_~a" id))
          (reg (iterate-reg instr))
          (table nil)
          (index (find-index-name (iterate-name instr))))
    (format-code stream "vector_iter ~a;~%" vec)
    (flet ((create-list-loop ()
             (format-code stream "for(auto ~a(~a->begin()), ~aend(~a->end()); ~a != ~aend; ~a++) {~%" it lsname it lsname it it it)
             (with-tab
                 (format-code stream "tuple *~a(*~a);~%" tpl it)
                 (create-c-matches-code stream tpl def (iterate-matches instr) allocated-tuples "continue;")
                 (c-iterate-add-vector stream tpl lsname it vec))
             (format-code stream "}~%")))
      (with-definition def (:name name :types types)
       (format-code stream "// iterate predicate ~a~%" (iterate-name instr))
       (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl predicate def))
       (format-code stream "{~%")
       (with-tab
       (cond
        ((null index)
         (format-code stream "auto *~a(~a->linear.get_linked_list(~a));~%" lsname node id)
         (create-list-loop))
        (t
          (setf table (generate-mangled-name "table"))
          (format-code stream "if(~a->linear.stored_as_hash_table(~a)) {~%" node predicate)
          (with-tab
           (format-code stream "hash_table *~a(~a->linear.get_hash_table(~a));~%" table node id)
           (format-code stream "if(~a != nullptr) {~%" table)
           (with-tab
            (let ((match (iterate-matches-constant-at-p (iterate-matches instr) (- (index-field index) 2))))
             (cond
              (match
               (let* ((value (match-right match))
                      (match-field (reg-dot-field (match-left match)))
                      (tuple-field (create-c-tuple-field-from-val stream allocated-tuples variables (arg-type (nth match-field types)) value)))
                (format-code stream "// search hash table for ~a~%" tuple-field)
                (format-code stream "utils::intrusive_list<vm::tuple> *~a(~a->lookup_list(~a));~%"
                 lsname table tuple-field)
                (create-list-loop)))
              (t
               (let ((it2 (generate-mangled-name "it")))
                (format-code stream "// go through hash table~%")
                (format-code stream "for(hash_table::iterator ~a(~a->begin()); !~a.end(); ++~a) {~%" it2 table it2 it2)
                (with-tab
                 (format-code stream "utils::intrusive_list<vm::tuple> *~a(*~a);~%" lsname it2)
                 (create-list-loop))
                (format-code stream "}~%"))))))
              (format-code stream "}~%")))
           (format-code stream "} else {~%")
           (with-tab
            (format-code stream "auto *~a(~a->linear.get_linked_list(~a));~%" lsname node id)
            (create-list-loop))
            (format-code stream "}~%")))
         (format-code stream "}~%")))
      (c-iterate-order-facts stream instr predicate vec)
      (let* ((itv (generate-mangled-name "it"))
             (obj (generate-mangled-name "obj"))
             (start-loop (if needs-label-p (generate-mangled-name "loop")))
             (frame (make-instance 'frame
                           :list lsname
                           :tuple tpl
                           :iterator it
                           :definition def
                           :ordered vec
                           :predicate predicate
                           :reg reg
                           :hash table
                           :is-linear-p has-removes-p
                           :start-loop start-loop)))
         (format-code stream "for(auto ~a(~a.begin()), ~aend(~a.end()); ~a != ~aend; ++~a) {~%" itv vec itv vec itv itv itv)
         (with-tab
          (with-separate-c-context (variables allocated-tuples)
            (format-code stream "iter_object ~a(*~a);~%" obj itv)
            (format-code stream "auto ~a(~a.iterator);~%" it obj)
            (format-code stream "auto ~a(~a.ls);~%" lsname obj)
            (format-code stream "vm::tuple *~a(~a.tpl);~%" tpl obj)
            (with-debug stream "DEBUG_ITER"
             (format-code stream "std::cout << \"\\titerate \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%" tpl predicate))
            (format-code stream "{~%")
            (with-tab
             (dolist (inner (iterate-instrs instr))
              (do-output-c-instr stream inner (cons frame frames) allocated-tuples variables :is-linear-p (or is-linear-p has-removes-p))))
             (format-code stream "}~%")
             (when needs-label-p
              (format-code stream "~a:continue;~%" start-loop))))
         (format-code stream "}~%"))))

(defun compile-c-order-persistent-iterate (stream instr frames variables allocated-tuples node &key is-linear-p)
 (let* ((def (lookup-definition (iterate-name instr)))
        (id (lookup-def-id (iterate-name instr)))
        (it (generate-mangled-name "it"))
        (tpl (generate-mangled-name "tpl"))
        (predicate (tostring "pred_~a" id))
        (reg (iterate-reg instr))
        (vec (generate-mangled-name "vec"))
        (frame (make-instance 'frame
                :list nil
                :tuple tpl
                :iterator it
                :definition def
                :predicate predicate
                :reg reg
                :is-linear-p nil
                :start-loop nil)))
  (with-definition def (:name name :types types)
   (format-code stream "// iterate through predicate ~a~%" (iterate-name instr))
   (format-code stream "vector_leaves ~a;~%" vec)
   (format-code stream "{~%")
   (with-tab
      (format-code stream "db::tuple_trie::tuple_search_iterator ~a(~a->pers_store.match_predicate(~a->get_id(), nullptr));~%" it node predicate)
      (format-code stream "for(auto ~aend(tuple_trie::match_end()); ~a != ~aend; ++~a) {~%" it it it it)
      (with-tab
        (format-code stream "tuple_trie_leaf *~aleaf(*~a);~%" tpl it)
        (format-code stream "tuple *~a(~aleaf->get_underlying_tuple());~%" tpl tpl)
        (create-c-matches-code stream tpl def (iterate-matches instr) allocated-tuples)
        (format-code stream "~a.push_back(~aleaf);~%" vec tpl))
      (format-code stream "}~%"))
   (format-code stream "}~%"))
   (let ((sub (order-iterate-subgoal instr)))
    (cond
     ((subgoal-has-random-p sub)
      (format-code stream "utils::shuffle_vector(~a, state.randgen);~%" vec))
     ((subgoal-has-min-p sub)
      (format-code stream "sort(~a.begin(), ~a.end(), tuple_leaf_sorter(~a, ~a));~%" vec vec  (subgoal-get-min-variable-position sub) predicate))
     (t (assert nil))))
   (format-code stream "for(auto ~a(~a.begin()); ~a != ~a.end(); ) {~%" it vec it vec)
   (with-tab
    (with-separate-c-context (variables allocated-tuples)
     (format-code stream "tuple_trie_leaf *~aleaf(*~a);~%" tpl it)
     (format-code stream "vm::tuple *~a(~aleaf->get_underlying_tuple());~%" tpl tpl)
     (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl predicate def))
     (with-debug stream "DEBUG_ITER"
      (format-code stream "std::cout << \"\\titerate \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%" tpl predicate))
     (dolist (inner (iterate-instrs instr))
      (do-output-c-instr stream inner (cons frame frames) allocated-tuples variables :is-linear-p is-linear-p))
     (format-code stream "~a = ~a.erase(~a);~%" it vec it)))
   (format-code stream "}~%")))

(defun compile-c-linear-iterate (stream instr frames variables allocated-tuples node &key is-linear-p has-removes-p)
   (let* ((def (lookup-definition (iterate-name instr)))
          (types (definition-types def))
          (id (lookup-def-id (iterate-name instr)))
          (lsname (generate-mangled-name "ls"))
          (it (generate-mangled-name "it"))
          (needs-label-p (and has-removes-p (not is-linear-p)))
          (tpl (generate-mangled-name "tpl"))
          (predicate (tostring "pred_~a" id))
          (reg (iterate-reg instr))
          (index (find-index-name (iterate-name instr))))
    (flet ((create-list-loop (hash match-field)
            (let* ((start-loop (if needs-label-p (generate-mangled-name "loop")))
                   (frame (make-instance 'frame
                           :list lsname
                           :tuple tpl
                           :iterator it
                           :definition def
                           :predicate predicate
                           :reg reg
                           :hash hash
                           :is-linear-p has-removes-p
                           :start-loop start-loop)))
             (format-code stream "for(auto ~a(~a->begin()), ~aend(~a->end()); ~a != ~aend; ) {~%" it lsname it lsname it it)
             (with-tab
              (with-separate-c-context (variables allocated-tuples)
                 (format-code stream "tuple *~a(*~a);~%" tpl it)
                 (let ((similar-tpls (locate-similar-tuples frames def)))
                  (dolist (sim similar-tpls)
                   (format-code stream "if(~a == ~a) {~%" tpl sim)
                   (with-tab
                    (format-code stream "~a++;~%" it)
                    (format-code stream "continue;~%"))
                   (format-code stream "}~%")))
                 (create-c-matches-code stream tpl def (iterate-matches instr) allocated-tuples (tostring "~a++; continue;" it) match-field)
                 (format-code stream "{~%")
                 (with-debug stream "DEBUG_ITER"
                     (format-code stream "std::cout << \"\\titerate \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%" tpl predicate))
                 (with-tab
                  (dolist (inner (iterate-instrs instr))
                   (do-output-c-instr stream inner (cons frame frames) allocated-tuples variables :is-linear-p (or is-linear-p has-removes-p))))
                 (format-code stream "}~%")))
             (format-code stream "~a++;~%" it)
             (when needs-label-p
              (format-code stream "~a:continue;~%" start-loop))
             (format-code stream "}~%"))))
      (with-definition def (:name name :types types)
       (format-code stream "// iterate predicate ~a~%" (iterate-name instr))
       (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl predicate def))
       (cond
        ((null index)
         (format-code stream "auto *~a(~a->linear.get_linked_list(~a));~%" lsname node id)
         (create-list-loop nil nil))
        (t
         (format-code stream "if(~a->linear.stored_as_hash_table(~a)) {~%" node predicate)
         (with-tab
          (let ((table (generate-mangled-name "table")))
           (format-code stream "hash_table *~a(~a->linear.get_hash_table(~a));~%" table node id)
           (format-code stream "if(~a != nullptr) {~%" table)
           (with-tab
            (let ((match (iterate-matches-constant-at-p (iterate-matches instr) (- (index-field index) 2))))
             (cond
              (match
               (let* ((value (match-right match))
                      (match-field (reg-dot-field (match-left match)))
                      (tuple-field (create-c-tuple-field-from-val stream allocated-tuples variables (arg-type (nth match-field types)) value)))
                (format-code stream "// search hash table for ~a~%" tuple-field)
                (format-code stream "utils::intrusive_list<vm::tuple> *~a(~a->lookup_list(~a));~%"
                 lsname table tuple-field)
                (create-list-loop table match-field)))
              (t
               (let ((it2 (generate-mangled-name "it")))
                (format-code stream "// go through hash table~%")
                (format-code stream "for(hash_table::iterator ~a(~a->begin()); !~a.end(); ++~a) {~%" it2 table it2 it2)
                (with-tab
                 (format-code stream "utils::intrusive_list<vm::tuple> *~a(*~a);~%" lsname it2)
                 (create-list-loop table nil))
                (format-code stream "}~%"))))))
              (format-code stream "}~%")))
           (format-code stream "} else {~%")
           (with-tab
            (format-code stream "auto *~a(~a->linear.get_linked_list(~a));~%" lsname node id)
            (create-list-loop nil nil))
            (format-code stream "}~%")))))))

(defun make-c-struct (stream allocated-tuples variables instr)
 (let* ((typ (vm-make-struct-type instr))
        (subtypes (type-struct-list typ))
        (name (generate-mangled-name "struct"))
        (params (loop for sub in subtypes
                      for i from 0
                      collect (create-c-tuple-field-from-val stream allocated-tuples variables sub (make-vm-stack i)))))
  (format-code stream "runtime::struct1 *~a(runtime::struct1::create((vm::struct_type*)type_~a));~%" name (lookup-type-id typ))
  (loop for param in params
        for i from 0
        do (format-code stream "~a->set_data(~a, ~a);~%" name i param))
  name))

(defun add-c-persistent (stream instr variables allocated-tuples node)
 (let ((reg (vm-add-persistent-reg instr))
       (name (generate-mangled-name "p")))
  (multiple-value-bind (p found-p) (gethash (reg-num reg) allocated-tuples)
   (with-debug stream "DEBUG_SENDS"
    (format-code stream "std::cout << \"\\tadd persistent \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%"
     (allocated-tuple-tpl p) (allocated-tuple-pred p)))
   (format-code stream "full_tuple *~a(new vm::full_tuple(~a, ~a, state.direction, state.depth));~%"
    name (allocated-tuple-tpl p) (allocated-tuple-pred p))
   (format-code stream "~a->store.persistent_tuples.push_back(~a);~%" node name)
   (when *facts-generated* (format-code stream "state.persistent_facts_generated++;~%")))))

(defun c-update-tuple-field (stream tt to-field to-type new-value)
 (let ((old (generate-mangled-name "old")))
  (format-code stream "const vm::tuple_field ~a(~a->get_field(~a));~%" old
   (allocated-tuple-tpl tt) to-field)
  (format-code stream "~a->~a(~a, ~a);~%" (allocated-tuple-tpl tt)
   (type-to-tuple-set to-type) to-field new-value)
  (format-code stream "runtime::do_decrement_runtime(~a, type_~a->get_type(), state.gc_nodes);~%"
   old (lookup-type-id to-type))))

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
         (let* ((place (vm-convert-float-place instr))
                (dest (vm-convert-float-dest instr))
                (v (find-c-variable variables place)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables dest :type-float)
            (format-code stream "~a = (vm::float_val)~a;~%" (declare-c-variable var new-p) (c-variable-name v))
            (add-c-variable variables var))))
      (:fabs
         (let* ((place (vm-fabs-float instr))
                (dest (vm-fabs-dest instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables dest :type-float)
           (format-code stream "~a = fabs(~a);~%" (declare-c-variable var new-p) (c-variable-name (find-c-variable variables place)))
           (add-c-variable variables var))))
      (:struct-valfr
         (let* ((rs (vm-struct-val-from instr))
                (idx (vm-struct-val-idx instr))
                (typ (vm-struct-val-type instr))
                (rdest (vm-struct-val-to instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg rs)) allocated-tuples)
           (assert found-p)
           (multiple-value-bind (var new-p) (allocate-c-variable variables rdest typ)
               (format-code stream "~a = ~a->get_struct(~a)->get_data(~a).~a;~%" (declare-c-variable var new-p)
                (allocated-tuple-tpl tpl) (reg-dot-field rs) idx (type-to-union-field typ))
               (add-c-variable variables var)))))
      (:structf
         (let* ((to (vm-make-struct-to instr))
                (name (make-c-struct stream allocated-tuples variables instr)))
          (multiple-value-bind (tuple found-p) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
           (assert found-p)
           (format-code stream "~a->set_struct(~a, ~a);~%" (allocated-tuple-tpl tuple) (reg-dot-field to) name))))
      (:structr
         (let* ((to (vm-make-struct-to instr))
                (name (make-c-struct stream allocated-tuples variables instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables to (vm-make-struct-type instr))
              (format-code stream "~a = ~a;~%" (declare-c-variable var new-p) name)
              (add-c-variable variables var))))
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
         (let* ((from (move-from instr))
                (to (move-to instr))
                (to-field (reg-dot-field to))
                (from-field (reg-dot-field from)))
          (multiple-value-bind (ft found1) (gethash (reg-num (reg-dot-reg from)) allocated-tuples)
            (assert found1)
            (multiple-value-bind (tt found2) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
             (assert found2)
             (let ((to-type (arg-type (nth to-field (definition-types (allocated-tuple-definition tt))))))
                (cond
                 ((and (reference-type-p to-type) (reg-dot-update-p to))
                  (c-update-tuple-field stream tt to-field to-type (tostring "~a->~a(~a)" (allocated-tuple-tpl ft) (type-to-tuple-get to-type) from-field)))
                 (t
                    (format-code stream "~a->~a(~a, ~a->~a(~a));~%" (allocated-tuple-tpl tt)
                              (type-to-tuple-set to-type) to-field (allocated-tuple-tpl ft) (type-to-tuple-get to-type) from-field))))))))
      (:cons-rff
         (let* ((tail (vm-cons-tail instr))
                (head (vm-cons-head instr))
                (dest (vm-cons-dest instr))
                (typ (vm-cons-type instr)))
          (multiple-value-bind (tailt found1) (gethash (reg-num (reg-dot-reg tail)) allocated-tuples)
           (assert found1)
            (multiple-value-bind (destt found3) (gethash (reg-num (reg-dot-reg dest)) allocated-tuples)
             (assert found3)
             (let ((head-data (create-c-tuple-field-from-val stream allocated-tuples variables (type-list-element typ) head)))
             (format-code stream "~a->set_cons(~a, runtime::cons::create((runtime::cons*)~a->get_cons(~a), ~a, (list_type*)type_~a));~%"
               (allocated-tuple-tpl destt) (reg-dot-field dest)
               (allocated-tuple-tpl tailt) (reg-dot-field tail) head-data
               (lookup-type-id typ)))))))
      (:cons-frf
         (let* ((tail (vm-cons-tail instr))
                (head (vm-cons-head instr))
                (dest (vm-cons-dest instr))
                (typ (vm-cons-type instr))
                (tailv (find-c-variable variables tail)))
          (multiple-value-bind (headt found1) (gethash (reg-num (reg-dot-reg head)) allocated-tuples)
           (assert found1)
           (multiple-value-bind (destt found3) (gethash (reg-num (reg-dot-reg dest)) allocated-tuples)
            (assert found3)
            (format-code stream "~a->set_cons(~a, runtime::cons::create(~a, ~a->get_field(~a), (list_type*)type_~a));~%"
             (allocated-tuple-tpl destt) (reg-dot-field dest)
             (c-variable-name tailv)
             (allocated-tuple-tpl headt) (reg-dot-field head)
             (lookup-type-id typ))))))
      (:cons-rrf
         (let ((tail (vm-cons-tail instr))
               (head (vm-cons-head instr))
               (dest (vm-cons-dest instr)))
          (multiple-value-bind (tailt found1) (gethash (reg-num tail) variables)
           (assert found1)
           (multiple-value-bind (headt found2) (gethash (reg-num head) variables)
            (assert found2)
            (multiple-value-bind (destt found3) (gethash (reg-num (reg-dot-reg dest)) allocated-tuples)
             (assert found3)
             (let ((head-data (create-c-tuple-field-from-val stream allocated-tuples variables (c-variable-type headt) head)))
             (format-code stream "~a->set_cons(~a, runtime::cons::create((runtime::cons*)~a, ~a, (list_type*)type_~a));~%"
               (allocated-tuple-tpl destt) (reg-dot-field dest)
               (c-variable-name tailt) head-data
               (lookup-type-id (vm-cons-type instr)))))))))
      (:cons-frr
         (let ((tail (vm-cons-tail instr))
               (head (vm-cons-head instr))
               (dest (vm-cons-dest instr)))
          (multiple-value-bind (headt found1) (gethash (reg-num (reg-dot-reg head)) allocated-tuples)
           (assert found1)
           (multiple-value-bind (tailt found2) (gethash (reg-num tail) variables)
            (assert found2)
            (multiple-value-bind (var new-p) (allocate-c-variable variables dest (vm-cons-type instr))
             (format-code stream "~a = runtime::cons::create((runtime::cons*)~a, ~a->get_field(~a), (list_type*)type_~a);~%"
               (declare-c-variable var new-p) (c-variable-name tailt) (allocated-tuple-tpl headt)
               (reg-dot-field head) (lookup-type-id (vm-cons-type instr)))
             (add-c-variable variables var))))))
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
             (format-code stream "~a->set_cons(~a, runtime::cons::create(~a->get_cons(~a), ~a->get_field(~a), (list_type*)type_~a));~%"
               (allocated-tuple-tpl destt) (reg-dot-field dest) (allocated-tuple-tpl tailt)
               (reg-dot-field tail) (allocated-tuple-tpl headt) (reg-dot-field head)
               (lookup-type-id (vm-cons-type instr))))))))
      (:cons-rrr
         (let* ((tail (vm-cons-tail instr))
                (head (vm-cons-head instr))
                (dest (vm-cons-dest instr))
                (tailv (find-c-variable variables tail))
                (typ (vm-cons-type instr))
                (field (create-c-tuple-field-from-val stream allocated-tuples variables (type-list-element typ) head)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables dest typ)
            (format-code stream "~a = runtime::cons::create(~a, ~a, (list_type*)type_~a);~%"
               (declare-c-variable var new-p) (c-variable-name tailv) field (lookup-type-id typ))
            (add-c-variable variables var))))
      (:move-nil-to-field
         (let ((to (move-to instr)))
          (multiple-value-bind (tp found) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
           (assert found)
           (format-code stream "~a->~a(~a, nullptr);~%" (allocated-tuple-tpl tp) (type-to-tuple-set (make-list-type :all))
               (reg-dot-field to)))))
      (:move-nil-to-reg
         (let ((r (move-to instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables r (make-list-type :all))
             (format-code stream "~a = nullptr;~%" (declare-c-variable var new-p))
             (add-c-variable variables var))))
      (:move-cpus-to-reg
         (let ((to (move-to instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables to :type-int)
           (format-code stream "~a = (vm::int_val)All->NUM_THREADS;~%" (declare-c-variable var new-p))
           (add-c-variable variables var))))
      (:move-host-id-to-reg
         (let ((to (move-to instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables to :type-addr)
            (format-code stream "~a = (vm::node_val)node;~%" (declare-c-variable var new-p))
            (add-c-variable variables var))))
      (:move-host-id-to-field
         (let ((to (move-to instr)))
            (multiple-value-bind (tp found) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
               (assert found)
               (format-code stream "~a->set_node(~a, (vm::node_val)node);~%" (allocated-tuple-tpl tp) (reg-dot-field to)))))
      (:move-argument-to-reg
         (let ((r (move-to instr))
               (arg (move-from instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables r :type-string)
            (format-code stream "~a =  All->get_argument(~a);~%" (declare-c-variable var new-p) (vm-argument-id arg))
            (add-c-variable variables var))))
      (:move-world-to-field
         (let ((to (move-to instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
           (format-code stream "~a->set_int(~a, All->DATABASE->nodes_total);~%" (allocated-tuple-tpl tpl) (reg-dot-field to)))))
      (:move-float-to-reg
         (let ((from (move-from instr))
               (to (move-to instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables to :type-float)
           (format-code stream "~a = (vm::float_val)~a;~%" (declare-c-variable var new-p) (vm-float-val from))
           (add-c-variable variables var))))
      (:move-float-to-stack
         (let ((flt (move-from instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables (move-to instr) :type-float)
            (format-code stream "~a = ~a;~%" (declare-c-variable var new-p) (vm-float-val flt))
            (add-c-variable variables var))))
      (:move-float-to-field
         (let ((flt (move-from instr))
               (f (move-to instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg f)) allocated-tuples)
           (format-code stream "~a->set_float(~a, ~a);~%" (allocated-tuple-tpl tpl) (reg-dot-field f) (vm-float-val flt)))))
      (:move-int-to-reg
         (let ((from (move-from instr))
               (to (move-to instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables to :type-int)
            (add-c-variable variables var)
            (format-code stream "~a = ~a;~%" (declare-c-variable var new-p) (vm-int-val from)))))
      (:move-addr-to-reg
         (let ((from (move-from instr))
               (to (move-to instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables to :type-addr)
            (add-c-variable variables var)
            (let ((name (generate-mangled-name "node")))
               (format-code stream "static const vm::node_val ~a((vm::node_val)All->DATABASE->find_node(~a));~%" name (vm-ptr-val from))
               (format-code stream "~a = ~a;~%" (declare-c-variable var new-p) name)
               (add-c-variable variables var)))))
      (:move-addr-to-field
         (let ((from (move-from instr))
               (f (move-to instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg f)) allocated-tuples)
           (assert found-p)
            (let ((name (generate-mangled-name "node")))
               (format-code stream "static const vm::node_val ~a((vm::node_val)All->DATABASE->find_node(~a));~%" name (vm-ptr-val from))
               (format-code stream "~a->set_node(~a, ~a);~%" (allocated-tuple-tpl tpl) (reg-dot-field f) name)))))
      (:move-int-to-field
         (let ((from (move-from instr))
               (to (move-to instr)))
          (multiple-value-bind (tpl found) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
            (assert found)
             (format-code stream "~a->set_int(~a, ~a);~%" (allocated-tuple-tpl tpl) (reg-dot-field to) (vm-int-val from)))))
      ((:call2 :call1 :call3)
         (create-c-call stream instr variables))
      (:not
         (let* ((place (vm-not-place instr))
                (dest (vm-not-dest instr))
                (v (find-c-variable variables place)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables dest :type-bool)
           (format-code stream "~a = !~a;~%" (declare-c-variable var new-p) (c-variable-name v))
           (add-c-variable variables var))))
      (:addr-not-equal (make-c-op stream variables instr :type-bool "!="))
      (:addr-equal (make-c-op stream variables instr :type-bool "=="))
      (:int-greater (make-c-op stream variables instr :type-bool ">"))
      (:int-greater-equal (make-c-op stream variables instr :type-bool ">="))
      (:int-lesser (make-c-op stream variables instr :type-bool "<"))
      (:int-lesser-equal (make-c-op stream variables instr :type-bool "<="))
      (:int-plus (make-c-op stream variables instr :type-int "+"))
      (:int-minus (make-c-op stream variables instr :type-int "-"))
      (:int-mul (make-c-op stream variables instr :type-int "*"))
      (:int-div (make-c-op stream variables instr :type-int "/"))
      (:float-plus (make-c-op stream variables instr :type-float "+"))
      (:float-minus (make-c-op stream variables instr :type-float "-"))
      (:float-mul (make-c-op stream variables instr :type-float "*"))
      (:float-div (make-c-op stream variables instr :type-float "/"))
      (:float-lesser (make-c-op stream variables instr :type-bool "<"))
      (:float-lesser-equal (make-c-op stream variables instr :type-bool "<="))
      (:float-greater (make-c-op stream variables instr :type-bool ">"))
      (:float-greater-equal (make-c-op stream variables instr :type-bool ">="))
      (:float-equal (make-c-op stream variables instr :type-bool "=="))
      (:float-not-equal (make-c-op stream variables instr :type-bool "!="))
      (:int-equal (make-c-op stream variables instr :type-bool "=="))
      (:int-not-equal (make-c-op stream variables instr :type-bool "!="))
      (:if-else
         (let* ((r (vm-if-reg instr))
                (spec (vm-if-else-spec instr)))
            (multiple-value-bind (v found) (gethash (reg-num r) variables)
             (assert found)
             (when (and spec (reg-p (vm-if-spec-dest spec)))
                (multiple-value-bind (var new-p) (allocate-c-variable variables (vm-if-spec-dest spec) (expr-type (vm-if-spec-expr spec)))
                   (when new-p
                     (format-code stream "~a;~%" (declare-c-variable var new-p))
                     (add-c-variable variables var))))
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
      (:callf
         (let* ((fun (lookup-function (vm-callf-name instr)))
                (typ (function-ret-type fun))
                (params (loop for arg in (function-args fun)
                              for r from 0
                              collect (multiple-value-bind (v found-p) (gethash r variables)
                                        (c-variable-name v)))))
          (multiple-value-bind (var new-p) (allocate-c-variable variables 'stack typ)
             (format-code stream "~a = function_~a(~{~a~^, ~});~%" (declare-c-variable var new-p) (vm-callf-name instr) params)
             (add-c-variable variables var))))
      (:move-reg-to-stack
         (let ((r (move-from instr)))
            (multiple-value-bind (v found-p) (gethash (reg-num r) variables)
               (format-code stream "return ~a;~%" (c-variable-name v)))))
      (:move-int-to-stack
         (let ((i (move-from instr))
               (to (move-to instr)))
          (cond
           ((= (vm-stack-offset to) 32)
            (format-code stream "return ~a;~%" (vm-int-val i)))
           (t
            (multiple-value-bind (var new-p) (allocate-c-variable variables to :type-int)
             (format-code stream "~a = ~a;~%" (declare-c-variable var new-p) (vm-int-val i))
             (add-c-variable variables var))))))
      (:move-constant-to-reg
         (let* ((r (move-to instr))
                (const (move-from instr))
                (name (vm-constant-name const))
                (const-obj (lookup-const name))
                (typ (constant-type const-obj)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables r typ)
           (format-code stream "~a = const_~a;~%" (declare-c-variable var new-p) (good-c-name name))
           (add-c-variable variables var))))
      (:move-constant-to-field-ref
         (let* ((const (move-from instr))
                (f (move-to instr))
                (name (vm-constant-name const))
                (const-obj (lookup-const name))
                (to-field (reg-dot-field f))
                (typ (constant-type const-obj)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg f)) allocated-tuples)
           (assert found-p)
           (cond
            ((and (reference-type-p typ) (reg-dot-update-p f))
             (c-update-tuple-field stream tpl to-field typ (good-c-name name)))
            (t
              (format-code stream "~a->~a(~a, const_~a);~%" (allocated-tuple-tpl tpl) (type-to-tuple-set typ)
                  to-field (good-c-name name)))))))
      (:move-constant-to-field
         (let* ((const (move-from instr))
                (f (move-to instr))
                (name (vm-constant-name const))
                (const-obj (lookup-const name))
                (to-field (reg-dot-field f))
                (typ (constant-type const-obj)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg f)) allocated-tuples)
           (assert found-p)
           (format-code stream "~a->~a(~a, const_~a);~%" (allocated-tuple-tpl tpl) (type-to-tuple-set typ)
                  to-field (good-c-name name)))))
      (:move-reg-to-constant
         (let* ((r (move-from instr))
                (const (move-to instr))
                (var (find-c-variable variables r))
                (name (vm-constant-name const))
                (const-obj (lookup-const name))
                (typ (constant-type const-obj)))
          (format-code stream "const_~a = ~a;~%" (good-c-name name) (c-variable-name var))
          (when (reference-type-p typ)
            (let ((field (create-c-tuple-field-from-val stream allocated-tuples variables typ r)))
               (format-code stream "runtime::increment_runtime_data(~a, type_~a->get_type());~%"
                  field (lookup-type-id typ))))))
      (:move-int-to-constant
         (let ((i (move-from instr))
               (const (move-to instr)))
          (format-code stream "const_~a = ~a;~%" (good-c-name (vm-constant-name const)) (vm-int-val i))))
      (:move-reg-to-reg
       (let* ((r (move-from instr))
              (from (find-c-variable variables r)))
        (multiple-value-bind (var new-p) (allocate-c-variable variables (move-to instr) (c-variable-type from))
         (format-code stream "~a = ~a;~%" (declare-c-variable var new-p) (c-variable-name from))
         (add-c-variable variables var))))
      ((:move-reg-to-field :move-reg-to-field-ref)
         (let* ((from (move-from instr))
                (to (move-to instr))
                (to-field (reg-dot-field to)))
          (multiple-value-bind (v found-p) (gethash (reg-num from) variables)
           (assert found-p)
            (multiple-value-bind (tp found2) (gethash (reg-num (reg-dot-reg to)) allocated-tuples)
              (let ((typ (arg-type (nth to-field (definition-types (allocated-tuple-definition tp))))))
               (assert found2)
               (cond
                ((and (reg-dot-update-p to) (reference-type-p typ))
                  (c-update-tuple-field stream tp to-field typ (c-variable-name v)))
                (t
                 (format-code stream "~a->~a(~a, (~a)~a);~%" (allocated-tuple-tpl tp)
                  (type-to-tuple-set typ) (reg-dot-field to) (type-to-c-type typ) (c-variable-name v)))))))))
      (:move-field-to-reg
         (let* ((from (move-from instr))
                (to (move-to instr)))
          (multiple-value-bind (tp found) (gethash (reg-num (reg-dot-reg from)) allocated-tuples)
           (assert found)
           (let ((def (allocated-tuple-definition tp)))
            (with-definition def (:types types)
             (let* ((typ (arg-type (nth (reg-dot-field from) types)))
                    (c-type (type-to-c-type typ)))
              (multiple-value-bind (var new-p) (allocate-c-variable variables to typ)
               (format-code stream "~a = ~a->~a(~a);~%"
                (declare-c-variable var new-p) (allocated-tuple-tpl tp) (type-to-tuple-get typ) (reg-dot-field from))
               (add-c-variable variables var))))))))
      (:facts-consumed
         (let ((dest (vm-facts-proved-dest instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables dest :type-int)
           (format-code stream "~a = state.linear_facts_consumed);~%" (declare-c-variable var new-p))
           (add-c-variable variables var))))
      (:send (let* ((to (send-to instr))
                    (from (send-from instr)))
              (multiple-value-bind (p found-p) (gethash (reg-num to) variables)
                  (multiple-value-bind (tp found2) (gethash (reg-num from) allocated-tuples)
                   (let ((def (allocated-tuple-definition tp)))
                    (flet ((send-linear ()
                            (format-code stream "node->store.add_generated(~a, ~a);~%" (allocated-tuple-tpl tp) (allocated-tuple-pred tp))
                            (with-debug stream "DEBUG_SENDS"
                               (format-code stream "std::cout << \"local send \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%"
                                 (allocated-tuple-tpl tp) (allocated-tuple-pred tp)))
                            (format-code stream "state.generated_facts = true;~%")
                            (when *facts-generated* (format-code stream "state.linear_facts_generated++;~%")))
                           (send-persistent ()
                            (let ((name (generate-mangled-name "stpl")))
                             (format-code stream "vm::full_tuple *~a(new vm::full_tuple(~a, ~a, state.direction, state.depth));~%"
                                                name (allocated-tuple-tpl tp) (allocated-tuple-pred tp))
                             (with-debug stream "DEBUG_SENDS"
                              (format-code stream "std::cout << \"local send \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%"
                               (allocated-tuple-tpl tp) (allocated-tuple-pred tp)))
                             (format-code stream "node->store.persistent_tuples.push_back(~a);~%" name)
                             (when *facts-generated* (format-code stream "state.persistent_facts_generated++;~%")))))
                     (cond
                      ((and (is-linear-p def) (is-reused-p def))
                       (format-code stream "if(state.direction == NEGATIVE_DERIVATION) {~%")
                       (with-tab
                        (format-code stream "vm::tuple::destroy(~a, ~a, state.gc_nodes);~%" (allocated-tuple-tpl tp) (allocated-tuple-pred tp)))
                       (format-code stream "} else {~%")
                       (with-tab (send-linear))
                       (format-code stream "}~%"))
                      (t
                       (format-code stream "if(node == (db::node*)~a) {~%" (c-variable-name p))
                       (with-tab (cond
                                  ((is-linear-p def) (send-linear))
                                  (t (send-persistent))))
                       (format-code stream "} else {~%")
                       (with-tab
                        (with-debug stream "DEBUG_SENDS"
                         (format-code stream "std::cout << \"\\tsend \"; ~a->print(std::cout, ~a); std::cout << \" to \" << ((db::node*)~a)->get_id() << std::endl;~%"
                          (allocated-tuple-tpl tp) (allocated-tuple-pred tp) (c-variable-name p)))
                        (format stream "#ifdef FACT_BUFFERING~%")
                        (format-code stream "auto it(state.facts_to_send.find((db::node*)~a));~%" (c-variable-name p))
                        (format-code stream "tuple_array *arr;~%")
                        (format-code stream "if(it == state.facts_to_send.end()) {~%")
                        (with-tab
                         (format-code stream "state.facts_to_send.insert(make_pair((db::node*)~a, tuple_array()));~%" (c-variable-name p))
                         (format-code stream "it = state.facts_to_send.find((db::node*)~a);~%" (c-variable-name p)))
                        (format-code stream "}~%")
                        (format-code stream "arr = &(it->second);~%")
                        (format-code stream "full_tuple info(~a, ~a, state.direction, state.depth);~%" (allocated-tuple-tpl tp) (allocated-tuple-pred tp))
                        (format-code stream "arr->push_back(info);~%")
                        (format stream "#else~%")
                        (format-code stream "state.sched->new_work(node, (db::node*)~a, ~a, ~a, state.direction, state.depth);~%"
                         (c-variable-name p) (allocated-tuple-tpl tp) (allocated-tuple-pred tp))
                        (format stream "#endif~%"))
                       (format-code stream "}~%")))))))))
      (:new-axioms
         (let ((axioms (vm-new-axioms-subgoals instr)))
            (do-subgoals axioms (:name name :args args :subgoal axiom)
               (let ((id (lookup-def-id name))
                     (def (lookup-definition name)))
                  (format-code stream "{~%")
                  (with-tab
                     (format-code stream "// add ~a(~a)~%" name args)
                     (format-code stream "tuple *tpl(vm::tuple::create(pred_~a));~%" id)
                     (loop for arg in args
                           for id from 0
                           do (output-c-axiom-argument stream arg id))
                     (cond
                      ((is-linear-p def)
                       (with-debug stream "DEBUG_SENDS"
                        (format-code stream "std::cout << \"\\tadd linear \"; tpl->print(std::cout, pred_~a); std::cout << std::endl;~%" id))
                       (format-code stream "node->add_linear_fact(tpl, pred_~a);~%" id))
                      (t
                       (with-debug stream "DEBUG_SENDS"
                        (format-code stream "std::cout << \"\\tadd persistent \"; tpl->print(std::cout, pred_~a); std::cout << std::endl;~%" id))
                       (format-code stream "node->store.persistent_tuples.push_back(new full_tuple(tpl, pred_~a, state.direction, state.depth));~%" id))))
                  (format-code stream "}~%")))))
      (:select-node
          (when (vm-select-node-empty-p instr)
           (return-from do-output-c-instr nil))
          (format-code stream "switch(node->get_id()) {~%")
          (with-tab
             (vm-select-node-iterate instr (n instrs)
              (format-code stream "case ~a: {~%" n)
              (with-separate-c-context (variables allocated-tuples)
                 (with-tab
                     (dolist (inner instrs)
                        (do-output-c-instr stream inner frames allocated-tuples variables :is-linear-p is-linear-p))))
              (format-code stream "}~%")))
          (format-code stream "}~%"))
      (:stop-program
         (format-code stream "if(scheduling_mechanism) sched::threads_sched::stop_flag = true;~%"))
      (:return-derived
         ;; locate first linear
         (let ((first-linear (if is-linear-p (locate-first-loop-linear-frame frames) nil)))
            (cond
             (first-linear
               (let ((it (frame-iterator first-linear))
                     (goto (frame-start-loop first-linear)))
                (format-code stream "goto ~a;~%" goto)))
             (t ))))
      (:alloc (let* ((tuple-id (lookup-def-id (vm-alloc-tuple instr)))
                     (def (lookup-definition (vm-alloc-tuple instr)))
                     (reg (vm-alloc-reg instr))
                     (pred (tostring "pred_~a" tuple-id))
                     (tpl (generate-mangled-name "tpl")))
                 (setf (gethash (reg-num reg) allocated-tuples) (make-allocated-tuple tpl pred def))
                 (format-code stream "tuple *~a(vm::tuple::create(~a));~%" tpl pred)))
      (:new-node (let* ((r (vm-new-node-reg instr)))
                  (multiple-value-bind (var new-p) (allocate-c-variable variables r :type-addr)
                     (format-code stream "~a = (vm::node_val)state.sched->create_node();~%" (declare-c-variable var new-p))
                     (add-c-variable variables var))))
      (:add-linear (let ((reg (vm-add-linear-reg instr)))
                    (multiple-value-bind (p found-p) (gethash (reg-num reg) allocated-tuples)
                     (with-debug stream "DEBUG_SENDS"
                      (format-code stream "std::cout << \"\\tadd linear \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%"
                       (allocated-tuple-tpl p) (allocated-tuple-pred p)))
                     (format-code stream "node->add_linear_fact(~a, ~a);~%" (allocated-tuple-tpl p) (allocated-tuple-pred p))
                     (when *facts-generated* (format-code stream "state.linear_facts_generated++;~%")))))
      (:add-persistent (add-c-persistent stream instr variables allocated-tuples "node"))
      (:add-thread-persistent (add-c-persistent stream instr variables allocated-tuples "thread_node"))
      (:update
       (let* ((reg (vm-update-reg instr))
              (frame (locate-loop-frame frames reg)))
         (multiple-value-bind (tp found) (gethash (reg-num reg) allocated-tuples)
            (format-code stream "// tuple ~a is updated now.~%" (allocated-tuple-tpl tp))
            (format-code stream "~a++;~%" (frame-iterator frame))
            (with-debug stream "DEBUG_SENDS"
             (format-code stream "std::cout << \"\\tupdate \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%" (allocated-tuple-tpl tp) (allocated-tuple-pred tp)))
            (format-code stream "node->matcher.register_predicate_update(~a);~%" (allocated-tuple-pred tp)))))
      (:push )
      (:pop )
      (:push-n )
      (:push-registers )
      (:pop-registers )
      (:move-pcounter-to-stack )
      (:move-stack-to-pcounter )
      (:move-stack-to-reg
         (let ((r (move-to instr)))
          (multiple-value-bind (v found) (gethash 'stack variables)
           (setf (gethash (reg-num r) variables) v))))
      (:move-stack-to-field
         (let ((f (move-to instr)))
          (multiple-value-bind (v found) (gethash 'stack variables)
            (multiple-value-bind (tpl found) (gethash (reg-num (reg-dot-reg f)) allocated-tuples)
             (assert found)
             (let* ((def (allocated-tuple-definition tpl))
                    (field (reg-dot-field f))
                    (typ (arg-type (nth field (definition-types def)))))
               (format-code stream "~a->~a(~a, ~a);~%" (allocated-tuple-tpl tpl) (type-to-tuple-set typ) (reg-dot-field f) (c-variable-name v)))))))
      (:run-action
         (let ((r (vm-run-action-reg instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num r) allocated-tuples)
           (format-code stream "switch(state.direction) {~%")
           (with-tab
            (format-code stream "case POSITIVE_DERIVATION:~%")
            (with-tab
             (format-code stream "All->MACHINE->run_action(state.sched, ~a, ~a, state.gc_nodes);~%" (allocated-tuple-tpl tpl) (allocated-tuple-pred tpl))
             (format-code stream "break;~%"))
            (format-code stream "case NEGATIVE_DERIVATION:~%")
            (with-tab
             (format-code stream "vm::tuple::destroy(~a, ~a, state.gc_nodes);~%" (allocated-tuple-tpl tpl) (allocated-tuple-pred tpl))
             (format-code stream "break;~%")))
           (format-code stream "}~%"))))
      (:enqueue-linear
         (let ((r (vm-enqueue-linear-reg instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num r) allocated-tuples)
           (with-debug stream "DEBUG_SENDS"
            (format-code stream "std::cout << \"\\tenqueue \"; ~a->print(std::cout, ~a); std::cout << std::endl;~%"
             (allocated-tuple-tpl tpl) (allocated-tuple-pred tpl)))
           (format-code stream "node->store.add_generated(~a, ~a);~%" (allocated-tuple-tpl tpl) (allocated-tuple-pred tpl))
           (format-code stream "state.generated_facts = true;~%")
           (when *facts-generated*
            (format-code stream "state.linear_facts_generated++;~%")))))
      (:set-static-here (format-code stream "state.sched->set_node_static(node);~%"))
      (:cpu-static
         (let* ((rnode (vm-cpu-static-node instr))
                (num (vm-cpu-static-dest instr))
                (var-node (find-c-variable variables rnode)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables num :type-int)
           (format-code stream "~a = ((db::node*)~a)->get_owner()->num_static_nodes();~%"
               (declare-c-variable var new-p) (c-variable-name var-node))
           (add-c-variable variables var))))
      (:test-nil
         (let* ((r (vm-test-nil-place instr))
                (dest (vm-test-nil-dest instr))
                (ls (find-c-variable variables r)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables dest :type-bool)
           (format-code stream "~a = runtime::cons::is_null(~a);~%" (declare-c-variable var new-p)
               (c-variable-name ls))
           (add-c-variable variables var))))
      (:head-fr
         (let ((f (vm-head-cons instr))
               (r (vm-head-dest instr))
               (typ (vm-head-type instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg f)) allocated-tuples)
            (multiple-value-bind (var new-p) (allocate-c-variable variables r typ)
               (format-code stream "~a = ~a->get_cons(~a)->get_head().~a;~%" (declare-c-variable var new-p)
                  (allocated-tuple-tpl tpl) (reg-dot-field f) (type-to-union-field typ))
               (add-c-variable variables var)))))
      (:head-rr
         (let* ((rls (vm-head-cons instr))
                (r (vm-head-dest instr))
                (ls (find-c-variable variables rls))
                (typ (vm-head-type instr)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables r typ)
            (format-code stream "~a = ~a->get_head().~a;~%" (declare-c-variable var new-p) (c-variable-name ls)
             (type-to-union-field typ))
            (add-c-variable variables var))))
      (:tail-fr
         (let ((f (vm-tail-cons instr))
               (r (vm-tail-dest instr))
               (typ (vm-tail-type instr)))
          (multiple-value-bind (tpl found-p) (gethash (reg-num (reg-dot-reg f)) allocated-tuples)
            (multiple-value-bind (var new-p) (allocate-c-variable variables r typ)
               (format-code stream "~a = ~a->get_cons(~a)->get_tail();~%" (declare-c-variable var new-p)
                  (allocated-tuple-tpl tpl) (reg-dot-field f))
               (add-c-variable variables var)))))
      (:tail-rr
         (let* ((rls (vm-tail-cons instr))
                (rd (vm-tail-dest instr))
                (typ (vm-tail-type instr))
                (ls (find-c-variable variables rls)))
          (multiple-value-bind (var new-p) (allocate-c-variable variables rd typ)
           (format-code stream "~a = ~a->get_tail();~%" (declare-c-variable var new-p)
                  (c-variable-name ls))
           (add-c-variable variables var))))
      (:remove (let ((reg (vm-remove-reg instr)))
                (multiple-value-bind (p found-p) (gethash (reg-num reg) allocated-tuples)
                  (let* ((frame (locate-loop-frame frames reg))
                         (def (frame-definition frame))
                         (pred (frame-predicate frame)))
                     (when (is-reused-p def)
                        (format-code stream "if(state.direction == POSITIVE_DERIVATION) {~%")
                        (with-tab
                           (format-code stream "node->store.persistent_tuples.push_back(full_tuple::remove_new(~a, ~a, state.depth));~%"
                               (allocated-tuple-tpl p) pred))
                        (format-code stream "}~%"))
                     (let* ((ls (frame-list frame))
                            (tbl (frame-hash frame))
                            (tpl (frame-tuple frame))
                            (pred (frame-predicate frame))
                            (it (frame-iterator frame)))
                        (format-code stream "~a = ~a->erase(~a);~%" it ls it)
                        (format-code stream "vm::tuple::destroy(~a, ~a, state.gc_nodes);~%" tpl pred)
                        (format-code stream "if(~a->empty()~a) node->matcher.empty_predicate(~a);~%" ls (if tbl (tostring " && ~a->empty()" tbl) "") pred))))))
      (:rlinear-iterate
       (compile-c-linear-iterate stream instr frames variables allocated-tuples "node" :is-linear-p is-linear-p :has-removes-p nil))
      (:linear-iterate
       (compile-c-linear-iterate stream instr frames variables allocated-tuples "node" :is-linear-p is-linear-p :has-removes-p t))
      (:thread-linear-iterate
       (compile-c-linear-iterate stream instr frames variables allocated-tuples "thread_node" :is-linear-p is-linear-p :has-removes-p t))
      (:persistent-iterate
       (compile-c-persistent-iterate stream instr frames variables allocated-tuples "node" :is-linear-p is-linear-p))
      (:thread-persistent-iterate
       (compile-c-persistent-iterate stream instr frames variables allocated-tuples "thread_node" :is-linear-p is-linear-p))
      (:order-linear-iterate
       (compile-c-order-linear-iterate stream instr frames variables allocated-tuples "node" :is-linear-p is-linear-p :has-removes-p t))
      (:order-rlinear-iterate
       (compile-c-order-linear-iterate stream instr frames variables allocated-tuples "node" :is-linear-p is-linear-p :has-removes-p nil))
      (:order-persistent-iterate
       (compile-c-order-persistent-iterate stream instr frames variables allocated-tuples "node" :is-linear-p is-linear-p))
      (:reset-linear (dolist (inner (vm-reset-linear-instrs instr))
                        (do-output-c-instr stream inner frames allocated-tuples variables :is-linear-p nil)))
      (:set-priority-here
         (let ((r (vm-set-priority-priority instr)))
            (multiple-value-bind (v found) (gethash (reg-num r) variables)
               (assert found)
               (format-code stream "if(scheduling_mechanism) {~%")
               (with-tab
                  (format-code stream "state.sched->set_node_priority(node, ~a);~%" (c-variable-name v)))
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
      (:set-affinity
         (let* ((rnode (vm-set-affinity-node instr))
                (rtarget (vm-set-affinity-target instr))
                (node (find-c-variable variables rnode))
                (target (find-c-variable variables rtarget)))
          (format-code stream "state.sched->set_node_affinity((db::node*)~a, (db::node*)~a);~%" (c-variable-name node) (c-variable-name target))))
      (:set-cpu-here
         (let* ((rcpu (vm-set-cpu-cpu instr))
                (cpu (find-c-variable variables rcpu)))
          (format-code stream "state.sched->set_node_cpu(node, (vm::int_val)~a);~%" (c-variable-name cpu))))
      (:is-static
       (let* ((node (vm-is-static-node instr))
              (dest (vm-is-static-dest instr))
              (n (find-c-variable variables node)))
        (multiple-value-bind (var new-p) (allocate-c-variable variables dest :type-bool)
         (format-code stream "~a = ((db::node*)~a)->is_static();~%" (declare-c-variable var new-p) (c-variable-name n))
         (add-c-variable variables var))))
      (:set-static
       (let* ((node (vm-set-static-node instr))
              (var (find-c-variable variables node)))
        (format-code stream "state.sched->set_node_static((db::node*)~a);~%" (c-variable-name var))))
      (:add-priority
         (let* ((rnode (vm-add-priority-node instr))
                (rprio (vm-add-priority-priority instr))
                (node (find-c-variable variables rnode))
                (prio (find-c-variable variables rprio)))
          (format-code stream "state.sched->add_node_priority((db::node*)~a, ~a);~%"
           (c-variable-name node) (c-variable-name prio))))
      (:set-default-priority
         (let* ((rnode (vm-set-default-priority-node instr))
                (rprio (vm-set-default-priority-priority instr))
                (node (find-c-variable variables rnode))
                (prio (find-c-variable variables rprio)))
          (format-code stream "state.sched->set_default_node_priority((db::node*)~a, ~a);~%" (c-variable-name node) (c-variable-name prio))))
      (:node-priority
        (let* ((rnode (vm-node-priority-node instr))
               (rdest (vm-node-priority-dest instr))
               (node (find-c-variable variables rnode)))
         (multiple-value-bind (var new-p) (allocate-c-variable variables rdest :type-float)
          (format-code stream "~a = ((db::node*)(~a))->get_priority();~%" (declare-c-variable var new-p) (c-variable-name node))
          (add-c-variable variables var))))
      (:cpu-id
       (let* ((rnode (vm-cpu-id-node instr))
              (rdest (vm-cpu-id-dest instr))
              (node (find-c-variable variables rnode)))
        (multiple-value-bind (var new-p) (allocate-c-variable variables rdest :type-int)
            (format-code stream "~a = ((db::node*)~a)->get_owner()->get_id();~%" (declare-c-variable var new-p)
             (c-variable-name node))
            (add-c-variable variables var))))
      (:schedule-next
       (let* ((rnode (vm-schedule-next-node instr))
              (node (find-c-variable variables rnode)))
        (format-code stream "state.sched->schedule_next((db::node*)~a);~%" (c-variable-name node))))
      (:set-default-priority-here
         (let* ((rprio (vm-set-default-priority-priority instr))
                (prio (find-c-variable variables rprio)))
          (format-code stream "state.sched->set_default_node_priority(node, ~a);~%" (c-variable-name prio))))
      (:set-moving-here (format-code stream "state.sched->set_node_moving(node);~%"))
      (:set-moving
       (let* ((rnode (vm-set-moving-node instr))
              (node (find-c-variable variables rnode)))
        (format-code stream "state.sched->set_node_moving((db::node*)~a);~%" (c-variable-name node))))
      (otherwise (warn "not implemented ~a" instr))))

(defun do-output-c-header (stream)
   (setf *name-counter* 0)
   (format-code stream "#define DEBUG_SENDS~%")
   (format-code stream "#define DEBUG_ITER~%")
   (format-code stream "#include \"interface.hpp\"~%")
   (format-code stream "#include \"external/others.hpp\"~%")
   (format-code stream "#include \"external/array.hpp\"~%")
   (format-code stream "#include \"external/lists.hpp\"~%")
   (format-code stream "#include \"external/math.hpp\"~%")
   (format-code stream "#include \"external/utils.hpp\"~%")
   (format-code stream "#include \"db/database.hpp\"~%")
   (format-code stream "#include \"db/node.hpp\"~%")
   (format-code stream "#include \"vm/program.hpp\"~%")
   (format-code stream "#include \"vm/state.hpp\"~%")
   (format-code stream "#include \"vm/tuple.hpp\"~%")
   (format-code stream "#include \"thread/threads.hpp\"~%")
   (format-code stream "#include \"machine.hpp\"~%")
   (format-code stream "~%")
   (format-code stream "using namespace vm;~%")
   (format-code stream "using namespace db;~%")
   (format-code stream "using namespace utils;~%")
   (format-code stream "~%")
   (format-code stream "#include \"vm/order.cpp\"~%")
   (format-code stream "~%")
   ;; static types
   (loop for typ in *program-types*
         for i from 0
         do (format-code stream "static vm::type *type_~a{nullptr};~%" i))
   ;; static predicates
   (do-definitions (:id i)
      (format-code stream "static vm::predicate *pred_~a{nullptr};~%" i))
   (format-code stream "~%")
   ;; static consts
   (format-code stream "// available consts in the program~%")
	(do-constant-list *consts* (:name name :type typ)
      (format-code stream "static ~a const_~a;~%" (type-to-c-type typ) (good-c-name name)))
   (format-code stream "~%")

   ;; add_definitions function.
   (format-code stream "void add_definitions(vm::program *prog) {~%")
   (with-tab
      (loop for typ in *program-types*
            for i from 0
            do (progn
                  (format-code stream "type_~a = " i)
                  (create-c-type stream typ)
                  (format stream ";~%")
                  (format-code stream "prog->add_type(type_~a);~%" i)))
      (format-code stream "~%")
      (format-code stream "prog->num_predicates_uint = next_multiple_of_uint(~a);~%" (length *definitions*))
      (format-code stream "prog->number_rules = ~a;~%" (length *code-rules*))
      (format-code stream "prog->number_rules_uint = next_multiple_of_uint(prog->num_rules());~%")
      (format-code stream "prog->num_args = ~a;~%" (args-needed *ast*))
      (format-code stream "prog->priority_order = ~a;~%" (case (get-priority-order) (:asc "PRIORITY_ASC") (:desc "PRIORITY_DESC")))
      (format-code stream "prog->initial_priority = initial_priority_value0(prog->priority_order == PRIORITY_DESC);~%")
      (format-code stream "prog->priority_static = ~a;~%" (if (get-priority-static) "true" "false"))
      (format-code stream "bitmap::create(prog->thread_predicates_map, prog->num_predicates_uint);~%")
      (do-definitions (:definition def :name name :types types :id id)
         (format-code stream "{~%")
         (with-tab
            (format-code stream "predicate *p(new predicate());~%")
            (format-code stream "pred_~a = p;~%" id)
            (format-code stream "p->id = ~a;~%" id)
            (format-code stream "p->is_linear = ~a;~%" (if (is-linear-p def) "true" "false"))
            (format-code stream "p->is_reverse_route = ~a;~%" (if (is-reverse-route-p def) "true" "false"))
            (format-code stream "p->is_action = ~a;~%" (if (is-action-p def) "true" "false"))
            (format-code stream "p->is_reused = ~a;~%" (if (is-reused-p def) "true" "false"))
            (format-code stream "p->is_thread = ~a;~%" (if (definition-is-thread-p def) "true" "false"))
            (format-code stream "p->has_code = ~a;~%" (if (and
                                                            (or (is-reused-p def) (not (is-linear-p def)))
                                                            (vm-find name))
                                                       "true" "false"))
            (when (definition-is-thread-p def)
               (format-code stream "prog->thread_predicates.push_back(p);~%")
               (format-code stream "prog->thread_predicates_map.set_bit(~a);~%" id))
            (when (definition-aggregate-p def)
               (let ((agg (definition-aggregate def)))
                (warn "agg ~a" agg)
                (format-code stream "p->agg_info = new predicate::aggregate_info;~%")
                (format-code stream "p->agg_info->safeness = AGG_UNSAFE;~%")
                (format-code stream "p->agg_info->field = ~a;~%" (position-if #'aggregate-p types))
                (format-code stream "p->agg_info->type = (vm::aggregate_type)~a;~%" (output-aggregate-type (aggregate-agg agg) (aggregate-type agg)))))
            (format-code stream "p->types.resize(~a);~%" (length types))
            (loop for typ in (mapcar #'arg-type types)
                  for i from 0
                  do (format-code stream "p->types[~a] = prog->get_type(~a);~%" i (lookup-type-id typ)))
            (format-code stream "p->name = \"~a\";~%" name)
            (format-code stream "p->cache_info(prog);~%")
            (let ((index (find-index-name name)))
               (when index
                (format-code stream "p->store_as_hash_table(~a);~%" (- (index-field index) 2))))
            (format-code stream "prog->add_predicate(p);~%"))
         (format-code stream "}~%"))
      ;; compile const code
      (let ((variables (create-variable-context))
            (allocated-tuples (create-allocated-tuples-context)))
       (loop for instr in *consts-code*
             do (do-output-c-instr stream instr nil allocated-tuples variables :is-linear-p nil)))
      (format-code stream "prog->sort_predicates();~%")
      (loop for code-rule in *code-rules*
            for count from 0
            do (let ((str (rule-string code-rule)))
                  (format-code stream "{~%")
                  (with-tab
                     (format-code stream "rule *r(new rule(~a, std::string(\"~a\")));~%" count (replace-all str (list #\newline) "\\n"))
                     (format-code stream "prog->rules.push_back(r);~%")
                     (loop for id in (subgoal-ids code-rule)
                           do (format-code stream "r->add_predicate(~a);~%" id)
                           do (format-code stream "prog->predicates[~a]->add_linear_affected_rule(r);~%" id)))
                  (format-code stream "}~%"))))
   (format-code stream "}~%~%")
   ;; create functions
	(loop for code in *function-code*
         for fun in *functions*
         do (let* ((variables (create-variable-context))
                   (allocated-tuples (create-allocated-tuples-context))
                   (param (loop for arg in (function-args fun)
                                for reg from 0
                                for typ = (var-type arg)
                                for name = (var-name arg)
                                do (add-c-variable variables (make-c-variable typ name (make-reg reg)))
                                collect (tostring "~a ~a" (type-to-c-type typ) name))))
               (format-code stream "static inline ~a function_~a(~{~a~^, ~}) {~%" (type-to-c-type (function-ret-type fun)) (function-name fun) param)
               (with-tab
                  (dolist (instr code)
                     (do-output-c-instr stream instr nil allocated-tuples variables :is-linear-p nil)))
               (format-code stream "}~%~%")))

   ;; create predicate code
   (do-processes (:name name :instrs instrs)
      (format-code stream "static inline void run_predicate_~a(state& state, vm::tuple *tpl, db::node *node, db::node *thread_node) {~%" name)
      (with-tab
         (format-code stream "(void)tpl; (void)node; (void)thread_node;~%")
         (let ((variables (create-variable-context))
               (allocated-tuples (create-allocated-tuples-context))
               (pred (tostring "pred_~a" (lookup-def-id name))))
            (setf (gethash 0 allocated-tuples) (make-allocated-tuple "tpl" pred (lookup-definition name)))
            (dolist (instr instrs)
               (do-output-c-instr stream instr nil allocated-tuples variables :is-linear-p nil))))
      (format-code stream "}~%~%"))

   ;; create rules
	(loop for code-rule in *code-rules*
			for count = 0 then (1+ count)
         for code = (rule-code code-rule)
         do (format-code stream "// ~a~%" (replace-all (rule-string code-rule) (list #\Newline) (tostring "~C//" #\Newline)))
			do (format-code stream "static inline void perform_rule~a(state& state, db::node *node, db::node *thread_node) {~%" count)
         do (setf *name-counter* 0)
         do (with-tab
               (format-code stream "(void)thread_node;~%")
               (let ((allocated-tuples (create-allocated-tuples-context))
                     (variables (create-variable-context)))
                  (dolist (instr code)
                     (do-output-c-instr stream instr nil allocated-tuples variables :is-linear-p nil))))
         do (format-code stream "}~%~%"))

   (format-code stream "void run_predicate(state *s, vm::tuple *tpl, db::node *node, db::node *thread_node, const vm::predicate_id pred) {~%")
   (with-tab
      (format-code stream "switch(pred) {~%")
      (with-tab
          (do-processes (:name name :instrs instrs)
            (let ((id (lookup-def-id name)))
               (format-code stream "case ~a: run_predicate_~a(*s, tpl, node, thread_node); break;~%" id name)))
          (format-code stream "default: abort(); break;~%"))
      (format-code stream "}~%"))
   (format-code stream "}~%~%")

   (format-code stream "void run_rule(state *s, db::node *n, db::node *t, const size_t rule) {~%")
   (with-tab
      (format-code stream "switch(rule) {~%")
      (with-tab
         (loop for rule in *code-rules*
               for count from 0
               do (format-code stream "case ~a: perform_rule~a(*s, n, t); break;~%" count count))
         (format-code stream "default: abort(); break;~%"))
      (format-code stream "}~%"))
   (format-code stream "}~%"))

(defun do-output-c-code (stream)
   (do-output-c-header stream))

(defun output-c-code (file &key (write-ast nil) (write-code nil))
   (let ((c-file (concatenate 'string file ".cpp")))
      (with-output-file (stream c-file)
         (do-output-c-code stream))))
