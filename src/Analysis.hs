{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Analysis where

import           Control.Arrow
import           Control.Category
import           Control.Lens
import           Data.Copointed
import           Data.Foldable
import           MealyMoore
import           Prelude          hiding (id, (.))



fillH :: ([Int], Int) -> [Int]
fillH (h, i) = h & ix i %~ (+1)

histo :: Monad m => MooreK m Int [Int]
histo = hoistMoore arr . feedback' $ Moore ([0, 0, 0, 0], liftA fillH)


printM :: Show a => MealyK IO a a
printM = liftK (\x -> print x >> return x)


printAndFillH :: MooreK IO Int [Int]
printAndFillH = printM ->| histo


printDivAndFillH = arr (mod 2) ->| printM ->| histo


elPts x = (*x) <$> [1..10]

test = liftK elPts ->| histo
