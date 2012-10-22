#!/bin/sh

BASEDIR=$(dirname $0)

exec racket -t $BASEDIR/assembler/vmas.rkt -- $@
