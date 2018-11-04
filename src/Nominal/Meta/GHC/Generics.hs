{-# LANGUAGE KindSignatures, RankNTypes #-}
module Nominal.Meta.GHC.Generics where

import GHC.Generics
import Nominal.Meta
import Nominal.Variable

class Generic a => NLambda_Generic a where
    nlambda_from :: WithMeta a -> WithMeta (Rep a x)
    nlambda_from = idOp from
    nlambda_to :: WithMeta (Rep a x) -> WithMeta a
    nlambda_to = idOp to

instance NLambda_Generic Bool
instance NLambda_Generic Ordering
instance NLambda_Generic [a]
instance NLambda_Generic (Maybe a)
instance NLambda_Generic (Either a b)
instance NLambda_Generic ()
instance NLambda_Generic (a, b)
instance NLambda_Generic (a, b, c)
instance NLambda_Generic (a, b, c, d)
instance NLambda_Generic (a, b, c, d, e)
instance NLambda_Generic (a, b, c, d, e, f)
instance NLambda_Generic (a, b, c, d, e, f, g)

class Generic1 f => NLambda_Generic1 f where
    nlambda_from1 :: Var a => WithMeta (f a) -> WithMeta (Rep1 f a)
    nlambda_from1 = idOp from1
    nlambda_to1 :: Var a => WithMeta (Rep1 f a) -> WithMeta (f a)
    nlambda_to1 = idOp to1

class Constructor c => NLambda_Constructor c where
    nlambda_conName :: WithMeta (t c (f :: * -> *) a) -> [Char]
    nlambda_conName = noMetaResOp conName
    nlambda_conFixity :: WithMeta (t c (f :: * -> *) a) -> Fixity
    nlambda_conFixity = noMetaResOp conFixity
    nlambda_conIsRecord :: WithMeta (t c (f :: * -> *) a) -> Bool
    nlambda_conIsRecord = noMetaResOp conIsRecord

class Datatype d => NLambda_Datatype d where
    nlambda_datatypeName :: WithMeta (t d (f :: * -> *) a) -> [Char]
    nlambda_datatypeName = noMetaResOp datatypeName
    nlambda_moduleName :: WithMeta (t d (f :: * -> *) a) -> [Char]
    nlambda_moduleName = noMetaResOp moduleName
    nlambda_isNewtype :: WithMeta (t d (f :: * -> *) a) -> Bool
    nlambda_isNewtype = noMetaResOp isNewtype

class Selector s => NLambda_Selector s where
    nlambda_selName :: WithMeta (t s (f :: * -> *) a) -> [Char]
    nlambda_selName = selName . value

nlambda_U1 :: WithMeta (U1 p)
nlambda_U1 = noMeta U1

nlambda_L1 :: forall (f :: * -> *) (g :: * -> *) p . WithMeta (f p) -> WithMeta ((:+:) f g p)
nlambda_L1 = idOp L1

nlambda_R1 :: forall (f :: * -> *) (g :: * -> *) p . WithMeta (g p) -> WithMeta ((:+:) f g p)
nlambda_R1 = idOp R1

(###:*:) :: forall (f :: * -> *) (g :: * -> *) p . (Var (f p), Var (g p)) => WithMeta (f p) -> WithMeta (g p) -> WithMeta ((:*:) f g p)
(###:*:) = renameAndApply2 (:*:)

nlambda_unK1 :: WithMeta (K1 i c p) -> WithMeta c
nlambda_unK1 = idOp unK1

nlambda_unM1 :: WithMeta (M1 i c f p) -> WithMeta (f p)
nlambda_unM1 = idOp unM1

nlambda_unPar1 :: WithMeta (Par1 p) -> WithMeta p
nlambda_unPar1 = idOp unPar1

nlambda_unRec1 :: WithMeta (Rec1 f p) -> WithMeta (f p)
nlambda_unRec1 = idOp unRec1
