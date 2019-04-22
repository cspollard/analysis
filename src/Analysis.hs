{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}


module Analysis where

import           Control.Arrow
import           Control.Category
import           Control.Lens
import           Data.Foldable
import           Data.Key
import           Data.Monoid      (Sum (..))
import           Moore
import           Prelude          hiding (id, (.))



fillH :: ([Int], Int) -> [Int]
fillH (h, i) = h & ix i %~ (+1)

histo :: Moore Int [Int]
histo = feedback $ moore' fillH [0, 0, 0, 0]

monoidal :: (Arrow arr, Monoid m) => Moore' arr m m
monoidal = feedback $ moore' (arr $ uncurry mappend) mempty

counter :: Moore () (Sum Int)
counter = const 1 >>| monoidal

sink :: Moore a ()
sink = moore' (const ()) ()


type IndexF f g = forall a b. (a -> b -> a) -> f a -> g b -> f a

indexMoore :: Functor f => IndexF f g -> f (Moore i o) -> Moore (g i) (f o)
indexMoore indexf ms =
  Moore ms (fmap poopF) (indexMoore indexf <<< indexf feedF ms)



histogram
  :: Adjustable f
  => f (Moore () (Sum Int)) -> Moore (Key f) (f (Sum Int))
histogram start = (,()) >>| indexMoore indexKeyed start


indexKeyed :: Adjustable f => IndexF f ((,) (Key f))
indexKeyed combine as (k, b) = adjust (flip combine b) k as


-- printM :: Show a => Kleisli IO a a
-- printM = Kleisli $ \x -> print x >> return x
--
--
-- printAndFillH :: MooreK IO Int [Int]
-- printAndFillH = printM ->| histo
--
--
-- printDivAndFillH = arr (mod 2) ->| printM ->| histo
--
--
-- elPts = Kleisli $ \x -> (*x) <$> [1..10]
--
-- test = elPts ->| histo
--
--
--
--
-- indexId :: IndexF Identity Identity
-- indexId f x y = f <$> x <*> y
--
--
--
--
--
-- -- type Pairing f g = (a -> b -> c) -> f a -> g b -> c
-- --
-- -- pair :: Pairing f g -> MealyF a b -> MealyF (f a) (g b)
-- -- pair p (Mealy arr) = Mealy $ \fa -> p
--
-- -- pair' :: Pairing f g -> Moore a b -> Moore (f a) (g b)
-- -- pair' p (Moore (a, m))
