#!/bin/sh

BASEDIR=$(dirname $0)

exec racket -t $BASEDIR/vm/vm.rkt -- $@
