{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | The module enables conditional expressions using the if... then... else.. construct with a formula condition. 
-- Option RebindableSyntax is required.
--
--  Thanks to Samuel Schlesinger (https://github.com/SamuelSchlesinger) 
--  for pointing out such functionality (https://github.com/szynwelski/nlambda/issues/3).

module Nominal.If (ifThenElse) where

import Nominal.Conditional
import Nominal.Formula
import Prelude

class IfThenElse c a where
  ifThenElse :: c -> a -> a -> a

instance Conditional a => IfThenElse Formula a where
  ifThenElse = ite

instance IfThenElse Bool a where
  ifThenElse True a _ = a
  ifThenElse False _ b = b
