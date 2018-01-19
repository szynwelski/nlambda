#!/bin/sh
ghc Run.hs -package ghc -dynamic && /usr/bin/time -f "%U" ./Run && /usr/bin/time -f "%U" ./Run 1
