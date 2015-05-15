#!/bin/sh

PROGRAM_DIR=$(dirname $PWD/$0)
TARGET=$1
LISP_MELD=$(realpath "$PROGRAM_DIR/..")
echo $LISP_MELD
mkdir -p $TARGET || exit 1
cd $TARGET || exit 1
if [ ! -f quicklisp.lisp ]; then
   wget http://beta.quicklisp.org/quicklisp.lisp || exit 1
fi
if [ ! -f setup.lisp ]; then
   sbcl --no-userinit --load quicklisp.lisp <<EOF
(quicklisp-quickstart:install :path ".")
EOF
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
old_dir=$PWD
cd dists/quicklisp/software/cl-yacc-* || exit 1
patch -p0 < $LISP_MELD/yacc-comments.patch || exit 1
cd $old_dir
sbcl --no-userinit --control-stack-size 128 --dynamic-space-size 2048 --noprint --noinform --load setup.lisp <<EOF
(require 'asdf)
(load "setup.lisp")
(push #p"$LISP_MELD/" asdf:*central-registry*)
(ql:quickload "cl-meld")
EOF

