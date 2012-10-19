#!/bin/sh

exec racket -t debugger/vmdb.rkt -- $@
