#!/bin/sh
cabal v2-configure -fTOTAL_ORDER
cabal v2-build
cabal v2-install
