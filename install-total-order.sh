#!/bin/sh
runhaskell Setup.hs configure --user -fTOTAL_ORDER
runhaskell Setup.hs build
runhaskell Setup.hs install
