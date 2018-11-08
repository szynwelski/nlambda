#!/bin/sh
cd "$(dirname "$0")/.."
ghc Tests/RunMetaPlugin.hs -package ghc -dynamic && /usr/bin/time -f "%U" ./Tests/RunMetaPlugin
