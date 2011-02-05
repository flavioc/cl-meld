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
   
counter(A, N + 1) :-
   counter(A, N), N <= 2, X = 2.
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
	val(B, D),   
   G = C + 3,
	C = 2,
	GF = 200,
	C + D < 3.
")

; G = C + 3
; C = 2 + D crasha

(defun compile-vm (code)
   (let ((ast (add-base-tuples (parse-meld code))))
      (setf ast (type-check ast))
      (localize ast)))
      
(defparameter *ast* (compile-vm *edge*))