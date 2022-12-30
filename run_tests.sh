#!/usr/bin/sh

set -e

cd tests
errors=()
for t in `ls *.jnq`; do
  if ! ../jnq $t; then
    errors+=("FAILED $t")
  fi
done

echo "FAILED: ${errors[*]}"

cd - > /dev/null