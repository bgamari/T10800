#!/bin/bash -e

source ~/.env/prefix/prefix.sh

run() {
    ver=$1
    use_component ghc $ver
    time ghc Test.hs -O
    touch Test.hs
    /usr/bin/time -o time-$ver ghc Test.hs -O -dshow-passes > ghc-$ver.log 2>&1
}

run 7.8.4
run 7.10.3
cat time-7.8.4 time-7.10.3
