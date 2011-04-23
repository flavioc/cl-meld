(in-package :cl-meld)

;; debug variables
(defvar *prog* nil)
(defvar *ast* nil)
(defvar *code* nil)

(defun localize-code (file)
   (let* ((ast (add-base-tuples (parse-meld-file file)))
          (topo (optimize-topology ast))
          (typechecked (type-check topo)))
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

;; this is to be removed... soon
(defun comp (prog &optional (out "base"))
   (meld-compile (concatenate 'string "/Users/flaviocruz/Projects/meld/progs/" prog ".meld")
                 (concatenate 'string "/Users/flaviocruz/Projects/meld/" out)))
                 
(defparameter *force* (comp "pagerank"))