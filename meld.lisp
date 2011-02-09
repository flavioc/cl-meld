(in-package :cl-meld)

(defparameter *basic-prog*
"
type fact(node, int, int, int, int).
type coiso(node, int).

fact(A, B + 1, B + 2, 2, 3) :-
   fact(A, B, B, C, C),
   coiso(A, B),
   2 > 3 + B.
   
coiso(A, 2) :-
   edge(A, B),
   coiso(B, 3).
")

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