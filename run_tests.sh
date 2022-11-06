#!/usr/bin/sh

cd tests
for t in `ls *.jnq`; do
  ../jnq $t
done
cd - > /dev/null