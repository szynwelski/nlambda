module Nominal.Meta.Modules where

import Nominal.Meta
import Nominal.Meta.Control.Exception.Base
import Nominal.Meta.Data.Either
import Nominal.Meta.Data.Either.Utils
import Nominal.Meta.Data.Foldable
import Nominal.Meta.Data.Functor
import Nominal.Meta.Data.Functor.Identity
import Nominal.Meta.Data.Map.Internal
import Nominal.Meta.Data.Maybe
import Nominal.Meta.Data.MultiMap
import Nominal.Meta.Data.OldList
import Nominal.Meta.Data.Semigroup
import Nominal.Meta.Data.Set.Base
import Nominal.Meta.Data.Set.Internal
import Nominal.Meta.Data.Traversable
import Nominal.Meta.Data.Tuple
import Nominal.Meta.GHC.Base
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Enum
import Nominal.Meta.GHC.Err
import Nominal.Meta.GHC.Float
import Nominal.Meta.GHC.Generics
import Nominal.Meta.GHC.Integer.Type
import Nominal.Meta.GHC.List
import Nominal.Meta.GHC.Num
import Nominal.Meta.GHC.Prim
import Nominal.Meta.GHC.Read
import Nominal.Meta.GHC.Real
import Nominal.Meta.GHC.Show
import Nominal.Meta.GHC.Tuple
import Nominal.Meta.GHC.Types
import Nominal.Meta.Nominal.Atoms.Signature
import Nominal.Meta.Nominal.Util.Read
import Nominal.Meta.Nominal.Variable
import Nominal.Meta.System.IO
import Nominal.Meta.Text.ParserCombinators.ReadPrec

modules :: [String]
modules = ["Nominal.Meta.Control.Exception.Base",
           "Nominal.Meta.Data.Either",
           "Nominal.Meta.Data.Either.Utils",
           "Nominal.Meta.Data.Foldable",
           "Nominal.Meta.Data.Functor",
           "Nominal.Meta.Data.Functor.Identity",
           "Nominal.Meta.Data.Map.Internal",
           "Nominal.Meta.Data.Maybe",
           "Nominal.Meta.Data.MultiMap",
           "Nominal.Meta.Data.OldList",
           "Nominal.Meta.Data.Semigroup",
           "Nominal.Meta.Data.Set.Base",
           "Nominal.Meta.Data.Set.Internal",
           "Nominal.Meta.Data.Traversable",
           "Nominal.Meta.Data.Tuple",
           "Nominal.Meta.GHC.Base",
           "Nominal.Meta.GHC.Classes",
           "Nominal.Meta.GHC.Enum",
           "Nominal.Meta.GHC.Err",
           "Nominal.Meta.GHC.Float",
           "Nominal.Meta.GHC.Generics",
           "Nominal.Meta.GHC.Integer.Type",
           "Nominal.Meta.GHC.List",
           "Nominal.Meta.GHC.Num",
           "Nominal.Meta.GHC.Prim",
           "Nominal.Meta.GHC.Read",
           "Nominal.Meta.GHC.Real",
           "Nominal.Meta.GHC.Show",
           "Nominal.Meta.GHC.Tuple",
           "Nominal.Meta.GHC.Types",
           "Nominal.Meta.Nominal.Atoms.Signature",
           "Nominal.Meta.Nominal.Util.Read",
           "Nominal.Meta.Nominal.Variable",
           "Nominal.Meta.System.IO",
           "Nominal.Meta.Text.ParserCombinators.ReadPrec"]
