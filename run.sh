#!/bin/sh

. ${ENVFILE:-./env.sh}

#guile --no-auto-compile -L $GUILE_CODE_LOAD_PATH run-td-gammon.scm $*
guile $GUILE_CODE_LOAD_PATH run-td-gammon.scm $*

