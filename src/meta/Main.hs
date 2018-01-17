import Sample
import Meta
import Prelude (print)

--main = print (showData x, showData (value nlambda_x), showData y, showData (value nlambda_y))
--main = print (one, nlambda_one)
--main = print (showW $ wx, showW $ value $ nlambda_wx)
--main = print (fst $ Pair 1 2, snd $ Pair 1 2, value nlambda_fst $ empty $ Pair 1 2, value nlambda_snd $ empty $ Pair 1 2)
--main = print (one'', two'', nlambda_one'', nlambda_two'')
--main = print (letx, nlambda_letx)
--main = print (showMaybe Nothing, showMaybe (Just ()), (value nlambda_showMaybe) nlambda_Nothing, (value nlambda_showMaybe) ((value nlambda_Just) (empty ())))
--main = print (nlambda_test1, nlambda_test2, nlambda_test3, nlambda_test4, nlambda_test5)
--main = print ((value nlambda_show) nlambda_True)
main = print (test, nlambda_test, test1, nlambda_test1)
