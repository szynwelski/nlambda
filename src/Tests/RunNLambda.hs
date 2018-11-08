{-# LANGUAGE CPP #-}

import Tests.NLambda

#define DO_TEST(number) do {print "============================= Test number ==================================="; print (test/**/number); print (nlambda_test/**/number/**/)}

main = do DO_TEST(1)
