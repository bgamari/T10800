#!/bin/bash -e

use_component ghc 7.8.4
time ghc Test.hs -O
touch Test.hs
time ghc Test.hs -O -dshow-passes

use_component ghc 7.10.3
time ghc Test.hs -O
touch Test.hs
time ghc Test.hs -O -dshow-passes
