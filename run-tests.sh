#!/bin/sh

for t in tests/test*.rkt
do
	raco test $t
done
