
(require 'asdf)
(push (truename ".") asdf:*central-registry*)
(ql:quickload "cl-meld")
