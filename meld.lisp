(in-package :cl-meld)

(defparameter *basic-prog*
"
type fact(node, int).

fact(A, 2) :-
   fact(A, B),
   C = 2.
")

"fact(A, B + 1) :-
   fact(A, B),
   2 > 3 + B,
   D < 4, D = 2 + F, F = 5 + G, G = 2 * 50."

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
   
counter(A, 3) :-
   counter(A, B1).
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
type val(node, int).
type coiso(node, int).
   
val(A, 1 + D) :-
	edge(A, B),
	edge(A, T),
	val(B, D),
	coiso(T, R),
	edge(T, S),
	coiso(A, 20),   
   G = C + 3,
	C = 2 + D,
	C + D < 3.
")

(defun compile-vm (code)
   (let ((ast (add-base-tuples (parse-meld code))))
      (localize (type-check ast))))
      
(defparameter *ast* (compile-ast (compile-vm *basic-prog*)))