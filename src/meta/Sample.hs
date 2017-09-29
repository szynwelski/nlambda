{-# OPTIONS_GHC -fplugin MetaPlugin #-}
module Sample where

--empty :: ()
--empty = ()

data Data = X | Y

x = X
y = Y

--newtype NewType a = NewType a
--newtyp = NewType X

--true = True
--
--one = 1
--
--one' :: Int
--one' = 1
--
--ide = id
--ide x = x

--ide_true = ide Data

--
--eq :: Eq a => a -> a -> Bool
--eq = (==)
--
--show' :: Show a => a -> String
--show' = show
--
--cons = const
--
--first = fst
--
--not_true = not True
--
--id_true = id True
--
--show_true :: String
--show_true = show True

--data T = MkT1 {size :: Int} | MkT2
--
--f (MkT1 x) = x
--f MkT2 = 0
--
--g = const 0
--
--ff c y = (if c then f else g) y

--lam x = x + 1

--s Nothing = False
--s (Just x) = x
--
--pair = (1,2)

--cons = (:)

--list = [1,2,3]
--list = [1..3]::[Int]
--list = [x+y | x <- [1,2,3], y <- [10,100,1000]]

--data Meta = Cons Int [Meta]

--pair = p 1 id
--
--p a b = (a,b)
