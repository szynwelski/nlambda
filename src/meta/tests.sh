#!/bin/sh
ghc Tests.hs -package ghc -dynamic && /usr/bin/time -f "%U" ./Tests
