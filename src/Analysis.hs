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



monoidal :: Monoid m => Moore m m
monoidal = accum (uncurry mappend) mempty

counter :: Moore a (Sum Int)
counter = const 1 >>| monoidal

sink :: Moore a ()
sink = const () >>| monoidal


type IndexF f g = forall a b. (a -> b -> a) -> f a -> g b -> f a

indexKeyed :: Adjustable f => IndexF f ((,) (Key f))
indexKeyed combine as (k, b) = adjust (flip combine b) k as



indexMoore :: Functor f => IndexF f g -> f (Moore i o) -> Moore (g i) (f o)
indexMoore indexf ms =
  Moore ms (fmap poopF) (indexMoore indexf <<< indexf feedF ms)


histogram :: Adjustable f => f (Moore a b) -> Moore (Key f, a) (f b)
histogram start = indexMoore indexKeyed start






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
