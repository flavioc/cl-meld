
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc)
 (:export :parse-meld
					:*code*))
