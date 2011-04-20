(in-package :cl-meld)

;; debug variables
(defvar *prog* nil)
(defvar *ast* nil)
(defvar *code* nil)

(defun localize-code (file)
   (let* ((ast (add-base-tuples (parse-meld-file file)))
          (topoliged (optimize-topology ast))
          (typechecked (type-check topoliged)))
      (setf *ast* (localize typechecked))))
      
(defun do-meld-compile (file out)
   (let* ((localized-ast (localize-code file))
          (compiled (compile-ast localized-ast)))
      (optimize-code localized-ast compiled)
      (setf *code* compiled)
      (output-code localized-ast compiled out)))

(defun meld-compile (file out)
   (handler-case (do-meld-compile file out)
      (yacc-parse-error (c) (format t "Parse error: ~a~%" c))
      (type-invalid-error (c) (format t "Type error: ~a~%" (text c)))
      (localize-invalid-error (c) (format t "Localization error: ~a~%" (text c)))
      (compile-invalid-error (c) (format t "Compile error: ~a~%" (text c)))
      (output-invalid-error (c) (format t "Output error: ~a~%" (text c)))))

(defparameter *selected-file* "pagerank.meld")
(defparameter *out* (meld-compile (concatenate 'string "/Users/flaviocruz/Projects/meld/progs/" *selected-file*)
                              "/Users/flaviocruz/Projects/meld/base"))