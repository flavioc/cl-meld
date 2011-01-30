
(defparameter *code* "
type a(node, catom).
type b(node, float).
type c(node).
type d(node).
type e(node).

a(A, A) :- b(A,B), d(A), F = B + G, G = 2, G == B.

c(Node) :-
	d(Node),
	e(Node),
	(1 + (2 - (3 * 3) )) < 3.
")

(defparameter *counter*
"
type counter(node, int).
   
counter(A, N + 1) :- counter(A, N), N <= 2.
")

(defun compile-vm (code)
   (let ((ast (parse-meld code)))
      (compile (localize (type-check ast)))))
      
(defparameter *ast* (compile-vm *code*))