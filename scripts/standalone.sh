#!/bin/sh

PROGRAM_DIR=$(dirname $PWD/$0)
TARGET=compile
LISP_MELD=$(realpath "$PROGRAM_DIR/..")
mkdir -p $TARGET || exit 1
cd $TARGET || exit 1
if [ ! -f quicklisp.lisp ]; then
   wget http://beta.quicklisp.org/quicklisp.lisp || exit 1
fi
if [ ! -f setup.lisp ]; then
   sbcl --no-userinit --load quicklisp.lisp <<EOF
(quicklisp-quickstart:install :path ".")
EOF
   [ $? -eq 0 ] || exit 1
fi
sbcl --no-userinit --load setup.lisp <<EOF
(ql:quickload "yacc")
(ql:quickload "cl-ppcre")
(ql:quickload "cl-lex")
(ql:quickload "arnesi")
(ql:quickload "alexandria")
(ql:quickload "flexi-streams")
(ql:quickload "cl-csv")
(ql:quickload "ieee-floats")
EOF
[ $? -eq 0 ] || exit 1

old_dir=$PWD
cd dists/quicklisp/software/cl-yacc-* || exit 1
patch -p0 < $old_dir/../../yacc-comments.patch || exit 1
cd $old_dir
sbcl --dynamic-space-size 4096 --no-userinit --noprint --noinform --load setup.lisp <<EOF
(require 'asdf)
(load "setup.lisp")
(push #p"$LISP_MELD/" asdf:*central-registry*)
(ql:quickload "cl-meld")
EOF
[ $? -eq 0 ] || exit 1

