(in-package :cl-meld)

(deftuple _init (:type-addr) :init-tuple)
(deftuple setprio (:type-addr :type-int) :action :linear)
(deftuple setcolor (:type-addr :type-int :type-int :type-int) :action :linear)
(deftuple setedgelabel (:type-addr :type-addr :type-string) :action :linear)
(deftuple write-string (:type-addr :type-string) :action :linear)
