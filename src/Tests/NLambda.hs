{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
{-# LANGUAGE BangPatterns, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable #-}

module Tests.NLambda where

import NLambda

----------------------------------------------------------------------------
-- Test Formula
----------------------------------------------------------------------------

test1 :: [Formula]
test1 = [true, false]
