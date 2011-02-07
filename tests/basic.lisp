(defparameter +test-prog0+
"
type fact(node).
   
fact(A) :-
   fact(A).
")

(defparameter +test-prog2+
"
type fact(node, int).

fact(A, 2 + B) :-
   fact(A, B).
")

(defparameter +test-prog3+
"
type fact(node, int).
   
fact(A, 2 + B) :-
   fact(A, B),
   edge(A, C).
")

(defparameter +test-prog4+
"
type fact(node, int).
   
fact(A, B + D) :-
   fact(A, B),
   edge(A, C),
   fact(C, D).
")

(deftest :test1 "Basic tests"
   (test-equal
     '(:DEFINITIONS
       ((:DEFINITION "___egde" (:TYPE-NODE :TYPE-NODE)
         (:ROUTE (:REVERSE-ROUTE "edge")))
        (:DEFINITION "edge" (:TYPE-NODE :TYPE-NODE) (:ROUTE))
        (:DEFINITION "fact" (:TYPE-NODE) NIL))
       :CLAUSES
       ((:CLAUSE ((:SUBGOAL "edge" ((:VAR X0 :TYPE-NODE) (:VAR X1 :TYPE-NODE))))
         ((:SUBGOAL "___egde" ((:VAR X1 :TYPE-NODE) (:VAR X0 :TYPE-NODE)))) (:ROUTE))
        (:CLAUSE ((:SUBGOAL "fact" ((:VAR A :TYPE-NODE))))
         ((:SUBGOAL "fact" ((:VAR A :TYPE-NODE)))) NIL)))
      (compile-vm +test-prog0+))
   
   (test-equal
      '(:DEFINITIONS
       ((:DEFINITION "___egde" (:TYPE-NODE :TYPE-NODE)
         (:ROUTE (:REVERSE-ROUTE "edge")))
        (:DEFINITION "edge" (:TYPE-NODE :TYPE-NODE) (:ROUTE))
        (:DEFINITION "fact" (:TYPE-NODE :TYPE-INT) NIL))
       :CLAUSES
       ((:CLAUSE ((:SUBGOAL "edge" ((:VAR X0 :TYPE-NODE) (:VAR X1 :TYPE-NODE))))
         ((:SUBGOAL "___egde" ((:VAR X1 :TYPE-NODE) (:VAR X0 :TYPE-NODE)))) (:ROUTE))
        (:CLAUSE ((:SUBGOAL "fact" ((:VAR A :TYPE-NODE) (:VAR B :TYPE-INT))))
         ((:SUBGOAL "fact"
           ((:VAR A :TYPE-NODE)
            (:PLUS (:INT 2 :TYPE-INT) (:VAR B :TYPE-INT) :TYPE-INT))))
         NIL)))
      (compile-vm +test-prog2+))
      
   (test-equal
      '(:DEFINITIONS
       ((:DEFINITION "___egde" (:TYPE-NODE :TYPE-NODE)
         (:ROUTE (:REVERSE-ROUTE "edge")))
        (:DEFINITION "edge" (:TYPE-NODE :TYPE-NODE) (:ROUTE))
        (:DEFINITION "fact" (:TYPE-NODE :TYPE-INT) NIL))
       :CLAUSES
       ((:CLAUSE ((:SUBGOAL "edge" ((:VAR X0 :TYPE-NODE) (:VAR X1 :TYPE-NODE))))
         ((:SUBGOAL "___egde" ((:VAR X1 :TYPE-NODE) (:VAR X0 :TYPE-NODE)))) (:ROUTE))
        (:CLAUSE
         ((:SUBGOAL "fact" ((:VAR A :TYPE-NODE) (:VAR B :TYPE-INT)))
          (:SUBGOAL "edge" ((:VAR A :TYPE-NODE) (:VAR C :TYPE-NODE))))
         ((:SUBGOAL "fact"
           ((:VAR A :TYPE-NODE)
            (:PLUS (:INT 2 :TYPE-INT) (:VAR B :TYPE-INT) :TYPE-INT))))
         NIL)))
       (compile-vm +test-prog3+))
       
    (test-equal
      '(:DEFINITIONS
       ((:DEFINITION "___egde" (:TYPE-NODE :TYPE-NODE)
         (:ROUTE (:REVERSE-ROUTE "edge")))
        (:DEFINITION "__mangledname1" (:TYPE-NODE :TYPE-INT) ((:ROUTED-TUPLE)))
        (:DEFINITION "edge" (:TYPE-NODE :TYPE-NODE) (:ROUTE))
        (:DEFINITION "fact" (:TYPE-NODE :TYPE-INT) NIL))
       :CLAUSES
       ((:CLAUSE ((:SUBGOAL "edge" ((:VAR X0 :TYPE-NODE) (:VAR X1 :TYPE-NODE))))
         ((:SUBGOAL "___egde" ((:VAR X1 :TYPE-NODE) (:VAR X0 :TYPE-NODE)))) (:ROUTE))
        (:CLAUSE
         ((:SUBGOAL "fact" ((:VAR C :TYPE-NODE) (:VAR D :TYPE-INT)))
          (:SUBGOAL "___egde" ((:VAR C :TYPE-NODE) (:VAR A :TYPE-NODE))))
         ((:SUBGOAL "__mangledname1" ((:VAR A :TYPE-NODE) (:VAR D :TYPE-INT))))
         (:ROUTE))
        (:CLAUSE
         ((:SUBGOAL "__mangledname1" ((:VAR A :TYPE-NODE) (:VAR D :TYPE-INT)))
          (:SUBGOAL "fact" ((:VAR A :TYPE-NODE) (:VAR B :TYPE-INT))))
         ((:SUBGOAL "fact"
           ((:VAR A :TYPE-NODE)
            (:PLUS (:VAR B :TYPE-INT) (:VAR D :TYPE-INT) :TYPE-INT))))
         NIL)))
      (compile-vm +test-prog4+)))