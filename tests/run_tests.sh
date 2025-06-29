#!/usr/bin/env sh

cd $(dirname $0)

TEST_DIR=$(pwd) ./cram.py *.t
