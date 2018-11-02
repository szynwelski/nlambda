module Nominal.Meta.Nominal.Atoms.Signature where

import Nominal.Atoms.Signature
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Read
import Nominal.Meta.GHC.Show

instance NLambda_Eq Relation

instance NLambda_Ord Relation

instance NLambda_Read Relation

instance NLambda_Show Relation
