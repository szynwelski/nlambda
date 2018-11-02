module Nominal.Meta.Data.Functor.Identity where

import Data.Functor.Identity
import Nominal.Meta.GHC.Base

instance NLambda_Applicative Identity

instance NLambda_Functor Identity

instance NLambda_Monad Identity

