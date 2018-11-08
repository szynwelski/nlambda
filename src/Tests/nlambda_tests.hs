#!/bin/sh
cd "$(dirname "$0")/.."
ghc Tests/RunNLambda.hs -package ghc -dynamic && /usr/bin/time -f "%U" ./Tests/RunNLambda
