#!/bin/sh
cd "$(dirname "$0")/.."
ghc Tests/RunNLambda.hs -package ghc -dynamic -DTOTAL_ORDER && /usr/bin/time -f "%U" ./Tests/RunNLambda
