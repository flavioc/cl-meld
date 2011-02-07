
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc :alexandria :arnesi :unit-test)
 (:export :parse-meld
					:*code*))

(arnesi:enable-sharp-l)