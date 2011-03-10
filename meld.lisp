(in-package :cl-meld)

(defparameter *basic-prog*
"
type fact(node, int).

fact(A, C) :-
   fact(A, B),
   edge(A, D),
   fact(D, 4),
   C = 2,
   B < C + 33.
")

(defparameter *init-prog*
"
type fact(node, int).

fact(A, 3).
fact(A, 4).
fact(A, 5), fact(A, 6 + 20 + 30).
")

(defparameter *extern-prog*
"
type fact(node, int).
extern int getvalue(int, int).
extern int myfunc(int).

const coiso = 2.

fact(A, getvalue(coiso + myfunc(3), 3 + 4)).
")

(defparameter *const-prog*
"
type fact(node, int).

const bahbah = 4 + 5.
const valconst = 3 + bahbah.

fact(A, valconst). 
")

(defparameter *float-prog*
"
type fact(node, float).
   
fact(A, 5.334).
")

"fact(A, B + 1) :-
   fact(A, B),
   2 > 3 + B,
   D < 4, D = 2 + F, F = 5 + G, G = 2 * 50."

(defparameter *code-str* "
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

(defparameter *tut*
"
type fact(node, int).
type other(node, int).
type another(node, int).
   
extern int list_push_tail_node(int, int).
   
other(A, 5).
another(A, list_push_tail_node(2, B)) :- other(A, B).
  
fact(A, B + 5) :- other(A, C), another(A, B), another(A, 42).
")

(defparameter *runnable*
"
type fact(node, int).
type other(node, int).
type nei(node, min int).
   
fact(A, 42).
   
other(A, B + 1) :- fact(A, B).
   
nei(A, C + 2) :-
   edge(A, B),
   fact(B, C).
")

(defparameter *ls-code*
"
type fact(node, list int).
type another(node, list int).
type other(node).
   
other(A).
another(A, [2, 3]).
   
fact(A, [5 | L]) :- another(A, [2 | L]), other(A).
")

"
another(A, L2) :-
   L1 = list_cons(3, nil),
   L2 = list_cons(2, L1).
      
fact(A, list_cons(5, L)) :-
   another(A, Ls),
   2 = list_head(Ls),
   L = list_tail(Ls),
   other(A).
   
[1, 2, 3 | L]
"

(defun localize-code (code)
   (let ((ast (add-base-tuples (parse-meld code))))
      (localize (type-check ast))))
      
(defparameter *prog* *runnable*)
(defparameter *ast* (localize-code *prog*))
(defparameter *code* (compile-ast *ast*))
(defparameter *output* (output-code *ast* *code* "base.bb"))