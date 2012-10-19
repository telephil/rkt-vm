#!/bin/sh

exec racket -t tests/run-tests.rkt -m -- tests/test-*.rkt

