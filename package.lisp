
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc :alexandria)
 (:export :parse-meld
					:*code*))
