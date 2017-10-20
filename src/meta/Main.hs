import Sample
import Meta
import Prelude hiding (fst, snd)

--main = print (showData x, showData $ value nlambda_x, showData y, showData $ value nlambda_y)
--main = print (one, nlambda_one)
--main = print (showW $ wx, showW $ value $ nlambda_wx)
--main = print (fst $ Pair 1 2, snd $ Pair 1 2, value nlambda_fst $ empty $ Pair 1 2, value nlambda_snd $ empty $ Pair 1 2)
--main = print (one'', two'', nlambda_one'', nlambda_two'')
main = print (letx, nlambda_letx)
