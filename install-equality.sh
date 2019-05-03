#!/bin/sh
runhaskell Setup.hs configure --user
runhaskell Setup.hs build --ghc-options "-package ghc -dynamic"
runhaskell Setup.hs install
