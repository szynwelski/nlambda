#!/bin/sh
runhaskell Setup.hs configure --user -fTOTAL_ORDER
runhaskell Setup.hs build --ghc-options "-package ghc -dynamic"
runhaskell Setup.hs install
