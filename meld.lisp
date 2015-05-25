(in-package :cl-meld)

(defun localize-code (file)
   (setf *name-counter* 0) ;; reset counter for auto-generated fact names
   (setf *var-counter* 0)
   (printdbg "Parsing file ~a" file)
   (let ((ast (parse-meld-file file)))
      (set-abstract-syntax-tree ast)
      (printdbg "Checking topology...")
      (optimize-topology)
      (printdbg "Typechecking...")
      (type-check)
      (ast-remove-unneeded-definitions ast)
      (agg-transformer)
      (printdbg "Localizing rules...")
      (localize)
      (stratify))
   *ast*)
                     
(defun do-meld-compile (file out &optional (is-data-p nil))
   (with-node-type-context
      (localize-code file)
      (printdbg "Compiling AST into VM instructions...")
      (let ((compiled (compile-ast))
            (compiled-rules (compile-ast-rules)))
         (setf *code* compiled)
         (setf *code-rules* compiled-rules)
         (printdbg "All compiled. Now optimizing result...")
         (optimize-code)
         (printdbg "Optimized. Writing byte-code to ~a.m" out)
         (if is-data-p
            (output-data-file out)
            (output-code out))
         (printdbg "Writing C++ code to ~a.cpp" out)
         (output-c-code out)
         (printdbg "All done."))
      t))

(defun meld-compile (file out &optional (is-data-p nil))
   (format t "==> Compiling file ~a~%      to ~a.m~%" file out)
   (handler-case
      (progn
         (do-meld-compile file out is-data-p)
         t)
      (file-not-found-error (c) (format t "File not found: ~a~%" (text c)) nil)
      (parse-failure-error (c) (format t "Parse error at line ~a: ~a~%" (line c) (text c)) nil)
      (expr-invalid-error (c) (format t "Expression error: ~a~%" (text c)) nil)
      (type-invalid-error (c) (format t "Type error: ~a~%" (text c)) nil)
      (localize-invalid-error (c) (format t "Localization error: ~a~%" (text c)) nil)
      (stratification-error (c) (format t "Stratification error: ~a~%" (text c)) nil)
      (compile-invalid-error (c) (format t "Compile error: ~a~%" (text c)) nil)
		(external-invalid-error (c) (format t "External functions: ~a~%" (text c)) nil)
      (output-invalid-error (c) (format t "Output error: ~a~%" (text c)) nil)))

(defun meld-compile-exit (file out &optional (is-data-p nil))
   (sb-ext:quit :unix-status (if (meld-compile file out is-data-p) 0 1)))

(defun meld-clear-variables ()
	(setf *ast* nil)
	(setf *code* nil)
	(setf *code-rules* nil))
	
(defun meld-compile-list (pairs)
   (loop for (in out) in pairs
         do (unless (meld-compile in out)
               (format t "PROBLEM COMPILING ~a~%" in)
					(meld-clear-variables)
					(sb-ext:gc :full t)
               (return-from meld-compile-list nil)))
   t)

;; this is to be removed... soon
      
(defun create-debug-file (prog ext)
   (concatenate 'string "/Users/flaviocruz/Projects/meld/" prog ext))

(defun comp (prog &optional (out nil))
	(let ((output-file (if out out (pathname-name (pathname prog)))))
   	(meld-compile (create-debug-file prog ".meld")
                 	(create-debug-file output-file ""))))

(defun comp-data (prog &optional (out nil))
	(let ((output-file (if out out (pathname-name (pathname prog)))))
		(meld-compile (create-debug-file prog ".meld")
         (create-debug-file output-file "") t)))
