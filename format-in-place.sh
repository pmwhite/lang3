#!/bin/env sh

if [ $# -ne 1 ]; then
  echo "Expected a single argument. Usage: format-in-place.sh FILE"
  exit 1
fi

contents=$(./main.exe format $1)

if [ $? -eq 0 ]; then
  printf "%s" "$contents" > $1
fi
