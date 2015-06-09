(in-package :cl-meld)

(defparameter *use-optimizations* t)
(defparameter *use-stratification* nil)

;(defparameter *ordering-type* :random)
(defparameter *ordering-type* :breadth)
;(defparameter *ordering-type* :naive)
;(defparameter *ordering-type* :in-file)

;; Enable index directives.
(defparameter *use-index* t)
;; Enable compact directives.
(defparameter *use-compact* t)
