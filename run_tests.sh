#!/usr/bin/bash

set -e

cd tests
errors=()
for t in `ls *.jnq`; do
  if ../jnq $t; then
    echo "ok ($t)"
  else
    echo "FAIL ($t)"
    errors+=("compile $t")
  fi
done

cd - > /dev/null

echo "FAILED: ${errors[*]}"
