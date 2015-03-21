(defpackage #:meld-asd
 (:use :cl :asdf))

(in-package :meld-asd)

(defsystem cl-meld
 :name "meld"
 :version "0.0"
 :author "Flavio Cruz"
 :description "Meld compiler"
 :depends-on (:cl-lex :yacc :arnesi :alexandria :flexi-streams :ieee-floats :cl-csv)
 :components ( (:file "conf"
                        :depends-on ("package"))
               (:file "external"
                        :depends-on ("package"
                                     "util"
												 "manip"))
               (:file "parser"
		 						:depends-on ("package"
		 						             "macros"
		 						             "manip"
		 						             "conf"
												 "directives"))
		 			(:file "directives"
								:depends-on ("package"
												 "macros"
												 "util"
												 "conf"
												 "context"))
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
		 			                      "macros"
												 "search"))
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
		 			                      "types"
                                     "aggtransformer"))
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
                                     "bt2c"
		 			                      "context"
		 			                      "stratification"))
		 			(:file "stratification"
		 			         :depends-on ("manip"
		 			                      "util"
		 			                      "macros"
		 			                      "localize"
		 			                      "typecheck"
		 			                      "conf"
												 "directives"
												 "context"))
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
		 			                      "external"
												 "models/base"))
               (:file "bt2c"
                        :depends-on ("output"
                                     "package"))
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
               (:file "aggtransformer"
                        :depends-on ("manip"
                                     "util"
                                     "macros"
                                     "ast"))
		 			(:file "topology"
		 			         :depends-on ("manip"
		 			                      "util"
		 			                      "conf"))
               (:file "snap-stanford"
                        :depends-on ("util"
                                     "topology"))
		 			(:file "print"
		 			         :depends-on ("package"
		 			                      "manip"
		 			                      "macros"))
	 						(:file "package")))

