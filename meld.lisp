(in-package :cl-meld)

(defparameter *extern-prog*
"
type fact(node, int).
extern int getvalue(int, int).
extern int myfunc(int).

const coiso = 2.

fact(A, getvalue(coiso + myfunc(3), 3 + 4)).
")

(defparameter *ls-code*
"
type fact(node, list int).
type another(node, list int).

fact(A, [2, 3, 1, 3, 4]).

another(A, [X + 1 | [X * 42, Y * 3 | [2 * 2]]]) :-
   edge(A, B),
   fact(B, [X | [P | [Z | [W | [Y]]]]]).

fact(A, nil) :-   
   another(A, [_, _, _, _]).
")


(defvar *prog* nil)
(defvar *ast* nil)
(defvar *code* nil)

(defun localize-code (code)
   (let* ((ast (add-base-tuples (parse-meld code)))
          (topoliged (optimize-topology ast))
          (typechecked (type-check topoliged)))
      (setf *ast* (localize typechecked))))
      
(defun do-meld-compile (code out)
   (let* ((localized (localize-code code))
          (compiled (compile-ast localized)))
      (setf *code* compiled)
      (output-code localized compiled out)))
      
(defun meld-compile (code out)
   (handler-case (do-meld-compile code out)
      (yacc-parse-error (c) (format t "~a~%" c))
      (type-invalid-error (c) (format t "~a~%" (text c)))))
      
(defun read-file (file)
   (with-open-file (str file
                        :direction :input
                        :if-does-not-exist :error)
      (reduce #L(concatenate 'string !1 !2 (list #\newline))
         (loop for line = (read-line str nil nil)
                while line
                collect line) :initial-value "")))
                
(defun meld-compile-file (file out)
   (meld-compile (read-file file) out))

(defparameter *selected-file* "base.meld")
(defparameter *out* (meld-compile-file (concatenate 'string "/Users/flaviocruz/Projects/meld/progs/" *selected-file*)
                              "/Users/flaviocruz/Projects/meld/base"))