(defpackage #:meld-asd
 (:use :cl :asdf))

(in-package :meld-asd)

(defsystem cl-meld
 :name "meld"
 :version "0.0"
 :author "Flavio Cruz"
 :description "Meld compiler"
 :depends-on (:cl-lex :yacc :arnesi :alexandria :unit-test :flexi-streams :ieee-floats :cl-funcpar)
 :components ( (:file "conf"
                        :depends-on ("package"))
               (:file "external"
                        :depends-on ("package"
                                     "util"))
               (:file "parser"
		 						:depends-on ("package"
		 						             "macros"
		 						             "manip"
		 						             "conf"))
		 			(:file "util"
		 			         :depends-on ("package"
		 			                       "macros"))
		 			(:file "manip"
		 			         :depends-on ("package"
		 			                      "util"
		 			                      "macros"
		 			                      "ast"
		 			                      "context"))
		 			(:file "search"
		 			         :depends-on ("package"
		 			                      "util"
		 			                      "manip"
		 			                      "macros"))
		 			(:file "transform"
		 			         :depends-on ("package"
		 			                      "util"
		 			                      "manip"
		 			                      "macros"))
		 			(:file "types"
		 			         :depends-on ("package"
		 			                      "util"
		 			                      "macros"))
		 			(:file "macros"
		 			         :depends-on ("package"))
		 			(:file "ast"
		 			         :depends-on ("context"
		 			                      "util"
		 			                      "types"))
		 			(:file "context"
		 			         :depends-on ("package"))
		 			(:file "typecheck"
		 			         :depends-on ("package"
		 			                      "manip"
		 			                      "macros"
		 			                      "types"))
		 			(:file "localize"
		 			         :depends-on ("package"
		 			                      "search"
		 			                      "macros"
		 			                      "transform"))
		 			(:file "vm"
		 			         :depends-on ("util"
		 			                      "macros"
		                               "manip"))
		 			(:file "compile"
		 			         :depends-on ("package"
		 			                      "manip"
		 			                      "macros"
		 			                      "vm"))
		 			(:file "meld"
		 			         :depends-on ("parser"
		 			                      "localize"
		 			                      "topology"
		 			                      "compile"
		 			                      "models/parallel"
		 			                      "optimize"
		 			                      "typecheck"
		 			                      "output"
		 			                      "context"
		 			                      "stratification"))
		 			(:file "stratification"
		 			         :depends-on ("manip"
		 			                      "util"
		 			                      "macros"
		 			                      "localize"
		 			                      "typecheck"
		 			                      "conf"))
		 			(:file "models/base"
		 			         :depends-on ("manip"
		 			                      "macros"
		 			                      "util"))
		 			(:file "models/parallel"
		 			         :depends-on ("models/base"))
		 			(:file "output"
		 			         :depends-on ("manip"
		 			                      "util"
		 			                      "compile"
		 			                      "vm"
		 			                      "topology"
		 			                      "localize"
		 			                      "stratification"
		 			                      "external"))
		 			(:file "optimize"
		 			         :depends-on ("manip"
		 			                      "util"
		 			                      "macros"
		 			                      "vm"
		 			                      "compile"))
		 			(:file "compare"
		 			         :depends-on ("manip"
		 			                      "util"
		 			                      "macros"
		 			                      "ast"))
		 			(:file "topology"
		 			         :depends-on ("manip"
		 			                      "util"
		 			                      "conf"))
		 			(:file "print"
		 			         :depends-on ("package"
		 			                      "manip"))
	 						(:file "package")))

