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

for t in `find . -iname "*.jnq"`; do
  echo "format $t"
  if ! ../jnq_format $t > /dev/null; then
    errors+=("format $t")
  fi
done

cd - > /dev/null

# for t in `find  ../../jnq/ -iname "*.jnq"`; do
#   echo "format $t"
#   if ! ./jnq_format $t; then
#     errors+=("format $t")
#   fi
# done

echo "FAILED: ${errors[*]}"
