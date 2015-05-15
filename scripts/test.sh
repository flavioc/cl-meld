#!/bin/sh

git clone http://github.com/flavioc/meld
LISP_MELD=$(realpath "$PWD/..")
DIR=$PWD/meld

sbcl --no-userinit --control-stack-size 128 --dynamic-space-size 2048 --noprint --noinform --load compile/setup.lisp <<EOF
(require 'asdf)
(load "compile/setup.lisp")
(push #p"$LISP_MELD/" asdf:*central-registry*)
(ql:quickload "cl-meld")
(let ((files (mapcar #'(lambda (in) (list (namestring in) "out"))
   (directory "$DIR/tests/progs/*.meld"))))
   (sb-ext:quit :unix-status (if (cl-meld:meld-compile-list files) 0 1)))
EOF
[ $? -eq 0 ] || exit 1
rm -f out.*
rm -rf meld
