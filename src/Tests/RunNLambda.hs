{-# LANGUAGE CPP #-}

import Tests.NLambda

#define DO_TEST(number) let (test, nlambda_test) = (show (test/**/number), show (nlambda_test/**/number/**/)) in do {print "============================= Test number ==================================="; print nlambda_test; if test == take (length test) nlambda_test then return () else (error "TEST FAILED")}

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