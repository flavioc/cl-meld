(defpackage #:meld-asd
 (:use :cl :asdf))

(in-package :meld-asd)

(defsystem cl-meld
 :name "meld"
 :version "0.0"
 :author "Flavio Cruz"
 :description "Meld compiler"
 :depends-on (:cl-lex :yacc :alexandria :arnesi :unit-test)
 :components ((:file "parser"
		 						:depends-on ("package"
		 						             "macros"
		 						             "manip"))
		 			(:file "util"
		 			         :depends-on ("package"
		 			                       "macros"))
		 			(:file "manip"
		 			         :depends-on ("package"
		 			                      "util"
		 			                      "macros"))
		 			(:file "macros"
		 			         :depends-on ("package"))
		 			(:file "typecheck"
		 			         :depends-on ("package"
		 			                      "manip"
		 			                      "macros"))
		 			(:file "localize"
		 			         :depends-on ("package"
		 			                      "manip"
		 			                      "macros"))
		 			(:file "compile"
		 			         :depends-on ("package"
		 			                      "manip"
		 			                      "macros"))
		 			(:file "meld"
		 			         :depends-on ("parser"
		 			                      "localize"
		 			                      "compile"
		 			                      "models/parallel"))
		 			(:file "models/base"
		 			         :depends-on ("manip"
		 			                      "macros"
		 			                      "util"))
		 			(:file "models/parallel"
		 			         :depends-on ("models/base"))
		 			(:file "print"
		 			         :depends-on ("package"
		 			                      "manip"))
	 						(:file "package")))

