(in-package :cl-meld)

(defparameter *use-optimizations* t)
(defparameter *use-stratification* nil)

;(defparameter *ordering-type* :random)
(defparameter *ordering-type* :breadth)
;(defparameter *ordering-type* :naive)
;(defparameter *ordering-type* :in-file)

;; Enable compact and index directives.
(defparameter *use-dsopt* t)
