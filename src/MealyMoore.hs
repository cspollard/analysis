{-# LANGUAGE Arrows        #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module MealyMoore where

import           Control.Arrow
import           Control.Category
import           Data.Bifunctor        hiding (first, second)
import           Data.Copointed
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Pointed
import           Data.Profunctor
import           Prelude               hiding (id, (.))


newtype Mealy arr i o = Mealy { feed :: arr i (Moore arr i o) }

newtype Moore arr i o = Moore { step :: (o, Mealy arr i o) }

type MealyF = Mealy (->)
type MealyK m = Mealy (Kleisli m)
type MooreF = Moore (->)
type MooreK m = Moore (Kleisli m)

-- fixity?
infixr 2 ->|
(->|) :: Arrow arr => Mealy arr i i' -> Moore arr i' o -> Moore arr i o
m ->| Moore (o, m') = Moore (o, m >>> m')


mealy = Mealy
moore b = Moore . (b,)


neglect :: Moore arr i o -> Mealy arr i o
neglect = step >>> snd

feedK :: MealyK m i o -> i -> m (MooreK m i o)
feedK m = runKleisli $ feed m

feedF :: MealyF i o -> i -> MooreF i o
feedF = feed


feedback :: Arrow arr => Mealy arr (b, a) b -> b -> Mealy arr a b
feedback (Mealy m) b =
  Mealy $ proc a -> do
    Moore (b', m') <- m -< (b, a)
    returnA -< Moore (b', feedback m' b')


feedback' :: Arrow arr => Moore arr (b, a) b -> Moore arr a b
feedback' (Moore (b, m)) = Moore (b, feedback m b)


-- Foldable is special to (->) and Kleisli somehow?
-- somehow this is particular to Hask

chompF :: Foldable f => MooreF a b -> MooreF (f a) b
chompF m@(Moore (b, _)) = Moore (b, Mealy f)
  where
    f xs = chompF $ foldl (\g x -> feed (neglect g) x) m xs


chompK :: (Foldable f, Monad m) => MooreK m i o -> MooreK m (f i) o
chompK m@(Moore (b, _)) = Moore (b, Mealy $ Kleisli f)
  where
    f xs = chompK <$> foldlM (\g x -> feedK (neglect g) x) m xs


hoistMealy
  :: (Profunctor arr, Profunctor arr')
  => (forall a b. arr a b -> arr' a b) -> Mealy arr i o -> Mealy arr' i o
hoistMealy f (Mealy arr) = Mealy (f $ rmap (hoistMoore f) arr)


hoistMoore
  :: (Profunctor arr, Profunctor arr')
  => (forall a b. arr a b -> arr' a b) -> Moore arr i o -> Moore arr' i o
hoistMoore f (Moore (o, arr)) = Moore (o, hoistMealy f arr)


generalize :: (Profunctor arr, Arrow arr) => MealyF i o -> Mealy arr i o
generalize = hoistMealy arr

generalize' :: (Profunctor arr, Arrow arr) => MooreF i o -> Moore arr i o
generalize' = hoistMoore arr

simplify :: MealyK Identity i o -> MealyF i o
simplify = hoistMealy $ fmap runIdentity <<< runKleisli

simplify' :: MooreK Identity i o -> MooreF i o
simplify' = hoistMoore $ fmap runIdentity <<< runKleisli



liftA :: Profunctor arr => arr a b -> Mealy arr a b
liftA p = Mealy $ rmap (\b' -> Moore . (b',) $ liftA p) p


liftF :: (a -> b) -> MealyF a b
liftF = liftA

liftF' :: (a -> b) -> b -> MooreF a b
liftF' f b = moore b $ liftF f


liftK :: Monad m => (a -> m b) -> MealyK m a b
liftK = liftA . Kleisli

liftK' :: Monad m => (a -> m b) -> b -> MooreK m a b
liftK' f b = moore b $ liftK f



instance Profunctor arr => Profunctor (Mealy arr) where
  dimap f g (Mealy m) = Mealy $ dimap f (dimap f g) m

instance Profunctor arr => Profunctor (Moore arr) where
  dimap f g (Moore om) = Moore $ bimap g (dimap f g) om


instance Profunctor arr => Functor (Mealy arr i) where
  fmap = rmap

instance Profunctor arr => Functor (Moore arr i) where
  fmap = rmap


instance Arrow arr => Category (Mealy arr) where
  id = r where r = Mealy . arr $ \i -> Moore (i, r)

  Mealy f . Mealy g = r
    where
      r = Mealy $ proc i -> do
            Moore (i', g') <- g -< i
            Moore (o', f') <- f -< i'
            returnA -< Moore (o', f' . g')


instance (Arrow arr) => Arrow (Mealy arr) where
  arr f = r where r = Mealy . arr $ \i -> Moore (f i, r)

  first (Mealy f) = Mealy $ proc (i, d) -> do
    Moore (o, m') <- f -< i
    returnA -< Moore ((o, d), first m')


instance Arrow arr => ArrowChoice (Mealy arr) where
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)


instance (Profunctor arr, Arrow arr) => Applicative (Mealy arr i) where
  pure o = Mealy . arr $ \i -> pure o

  Mealy mf <*> Mealy mx =
    Mealy $ proc i -> do
      mx' <- mx -< i
      mf' <- mf -< i
      returnA -< mf' <*> mx'


instance (Profunctor arr, Arrow arr) => Applicative (Moore arr i) where
  pure o = Moore (o, pure o)

  Moore (f, mf) <*> Moore (x, mx) = Moore (f x, mf <*> mx)


instance (Profunctor arr, Arrow arr) => Pointed (Mealy arr a) where
  point = pure

instance Copointed (Moore arr a) where
  copoint = fst . step
