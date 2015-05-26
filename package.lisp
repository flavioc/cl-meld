
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc :ieee-floats :cl-csv)
 (:import-from :flexi-streams :make-in-memory-output-stream)
 (:import-from :arnesi :sharpl-reader)
 (:export :comp :meld-compile :meld-compile-exit :meld-compile-list
          :snap-stanford-dump))

(arnesi:enable-sharp-l)
