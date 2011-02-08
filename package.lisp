
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc :arnesi :unit-test)
 (:import-from :alexandria :format-symbol :with-gensyms :when-let)
 (:export :parse-meld
					:*code*))

(arnesi:enable-sharp-l)