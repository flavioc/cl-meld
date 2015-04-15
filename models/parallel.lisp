(in-package :cl-meld)

(deftuple setcolor (:type-addr :type-int :type-int :type-int) :action :linear)
(deftuple setedgelabel (:type-addr :type-addr :type-string) :action :linear)
(deftuple write-string (:type-addr :type-string) :action :linear)
(deftuple setColor2 (:type-addr :type-int) :action)
(deftuple stop-program (:type-addr) :action :linear :instruction)
(deftuple set-priority (:type-addr :type-float) :action :linear :instruction)
(deftuple add-priority (:type-addr :type-float) :action :linear :instruction)
(deftuple schedule-next (:type-addr) :action :linear :instruction)
(deftuple set-default-priority (:type-addr :type-float) :action :linear :instruction)
(deftuple set-moving (:type-addr) :action :linear :instruction)
(deftuple set-static (:type-addr) :action :linear :instruction)
(deftuple set-affinity (:type-addr :type-addr) :action :linear :instruction)
(deftuple set-cpu (:type-addr :type-int) :action :linear :instruction)
(deftuple remove-priority (:type-addr) :action :linear :instruction)
(deftuple just-moved (:type-addr) :linear :special)
(deftuple thread-list (:type-thread (:type-list :type-thread)) :special)
(deftuple other-thread (:type-thread :type-thread :type-int) :special)
(deftuple leader-thread (:type-thread :type-thread) :special)
