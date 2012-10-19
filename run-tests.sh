#!/bin/sh

cd tests
exec racket -t run-tests.rkt
cd ..
