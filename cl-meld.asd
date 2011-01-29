
(defpackage #:meld-asd
 (:use :cl :asdf))

(in-package :meld-asd)

(defsystem cl-meld
 :name "meld"
 :version "0.0"
 :author "Flavio Cruz"
 :description "Meld compiler"
 :depends-on (:cl-lex :yacc)
 :components ((:file "parser"
		 						:depends-on ("package"))
		 			(:file "util"
		 			         :depends-on ("package"))
		 			(:file "manip"
		 			         :depends-on ("package"
		 			                      "util"))
		 			(:file "macros"
		 			         :depends-on ("package"
		 			                      "manip"))
		 			(:file "typecheck"
		 			         :depends-on ("package"
		 			                      "manip"
		 			                      "macros"))
		 			(:file "print"
		 			         :depends-on ("package"
		 			                      "manip"))
	 						(:file "package")))

