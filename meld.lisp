(in-package :cl-meld)

(defparameter *code* "
type a(node, catom).
type b(node, float).
type c(node).
type d(node).
type e(node).

a(A, A) :- b(A,B), d(A), F = B + G, G = 2, G = B.

c(Node) :-
	d(Node),
	e(Node),
	(1 + (2 - (3 * 3) )) < 3.
")

(defparameter *counter*
"type counter(node, int).
   
counter(A, N + 1) :- counter(A, N), N <= 2, X = 2.
")

(defparameter *tree*
"
type tree(node, node).
type root(node).
   
tree(Self, Self) :- root(Self).
tree(Self, Parent) :-
   edge(Self, Parent).
")

(defparameter *edge*
"
type fac(node, int).
   
fac(Self, Val + ValOther + 1) :-
   edge(Self, Other),
   fac(Other, Val),
   edge(Other, Another),
   fac(Another, ValOther).
")

(defun compile-vm (code)
   (let ((ast (add-base-tuples (parse-meld code))))
      (setf ast (type-check ast))
      (localize ast)))
      
(defparameter *ast* (compile-vm *code*))

