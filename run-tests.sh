#!/bin/sh

for t in tests/test*.rkt
do
    echo ">>> $t"
    raco test $t
done
