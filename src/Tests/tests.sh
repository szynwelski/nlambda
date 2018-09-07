#!/bin/sh
cd "$(dirname "$0")/.."
ghc Tests/Tests.hs -package ghc -dynamic && /usr/bin/time -f "%U" ./Tests/Tests
