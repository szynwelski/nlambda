{-# LANGUAGE CPP #-}

import qualified Data.Map as Map
import Nominal.Meta
import Nominal.Variable
import Tests.Test

withMeta :: a -> [(Identifier, Identifier)] -> WithMeta a
withMeta x = create x . metaFromMap . Map.fromList

x = setIdentifier 1 $ iterationVariable 0 1
y = setIdentifier 4 $ iterationVariable 0 2
m1 = withMeta x [(1,2)]
m2 = withMeta x [(1,3)]
m3 = withMeta y [(4,5)]

#define DO_TEST(number) do {print "============================= Test number ==================================="; print (test/**/number x x y); print (nlambda_test/**/number/**/ m1 m2 m3)}

main = do DO_TEST(1)
          DO_TEST(2)
          DO_TEST(3)
          DO_TEST(4)
          DO_TEST(5)
          DO_TEST(6)
          DO_TEST(7)
          DO_TEST(8)
          DO_TEST(9)
          DO_TEST(10)
          DO_TEST(11)
          DO_TEST(12)
          DO_TEST(13)
          DO_TEST(14)
          DO_TEST(15)
          DO_TEST(16)
          DO_TEST(17)
          DO_TEST(18)
          DO_TEST(19)
          DO_TEST(20)
          DO_TEST(21)
          DO_TEST(22)
          DO_TEST(23)
          DO_TEST(24)
          DO_TEST(25)
          DO_TEST(26)
          DO_TEST(27)
          DO_TEST(28)
          DO_TEST(29)
          DO_TEST(30)
          DO_TEST(31)
          DO_TEST(32)
          DO_TEST(33)
          DO_TEST(34)
          DO_TEST(35)
          DO_TEST(36)
          DO_TEST(37)
          DO_TEST(38)
          DO_TEST(39)
          DO_TEST(40)
          DO_TEST(42)
          DO_TEST(43)
          print "========================================================================="
