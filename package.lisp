
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc :arnesi :unit-test :ieee-floats)
 (:import-from :alexandria :format-symbol :with-gensyms :when-let)
 (:import-from :flexi-streams :make-in-memory-output-stream)
 (:export :parse-meld))

(arnesi:enable-sharp-l)