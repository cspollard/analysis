{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}


module Analysis where

import           Control.Arrow
import           Control.Category
-- import           Data.Align
import           Data.Coerce
import           Data.Foldable
import           Data.Key
import           Data.Map.Merge.Strict
import           Data.Map.Strict       (Map (..))
import           Data.Monoid           (Sum (..))
import           Data.Variation
import           MealyMoore
import           Prelude               hiding (id, (.))
import           Thread



monoidal :: Monoid m => Moore' m m
monoidal = feedback $ Moore (arr $ uncurry (<>)) mempty


counter :: Enum b => b -> Moore' a b
counter b = feedback <<< flip Moore b <<< arr $ \(b', a) -> succ b'


sink :: Moore' a ()
sink = pure ()


type IndexF f g = forall a b. (a -> b -> a) -> f a -> g b -> f a
-- type IndexA arr f g = forall a b. arr (a, b) a -> arr (f a, g b) f a


indexKeyed :: Adjustable f => IndexF f ((,) (Key f))
indexKeyed combine as (k, b) = adjust (flip combine b) k as

-- indexAKeyed :: IndexA arr f ((,) (Key f))
-- indexAKeyed


indexMoore :: Functor f => IndexF f g -> f (Moore' i o) -> Moore' (g i) (f o)
indexMoore indexf ms =
  Moore m (poop <$> ms)
    where
      m = Mealy $ indexf (curry chomp) ms >>> indexMoore indexf


histogram :: Adjustable f => f (Moore' i o) -> Moore' (Key f, i) (f o)
histogram = indexMoore indexKeyed



-- indexMooreK
--   :: (Monad m, Traversable f)
--   => IndexF f g -> f (MooreK i (m o)) -> MooreK m (g i) (f o)
-- indexMooreK indexf ms =
--   Moore
--     ms
--     (fmap poopK)
--     (indexMooreK indexf <<< indexf feedK ms)




type Var = Variation (Map String)

-- instance Align (Map k) where
--   align = coerce align


-- histogramV :: Monoid o => Moore' (Int, o) (Var [o])
-- histogramV =
--   fmap (toMap "nominal")
--   >>| histogram [monoidal, monoidal]
--   |>> traverse (fromMap "nominal")
--
--   where
--     toMap :: String -> Var a -> Map String a
--     toMap k (Variation n vs) = vs & at k .~ Just n
--
--     fromMap :: String -> Map String a -> Var a
--     fromMap k m = Variation (m ^?! ix k) (m & at k .~ Nothing)
