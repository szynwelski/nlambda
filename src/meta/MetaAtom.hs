module MetaAtom where

import Meta
import Data.Map

{-# ANN module "WithMeta" #-}

data Atom = Atom Int deriving Show

atom :: Atom
atom = Atom 0

atom_nlambda :: WithMeta Atom
atom_nlambda = create atom $ metaFromMap $ singleton 0 1
