{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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
import           Thread


newtype Mealy arr i o = Mealy { runMealy :: arr i (Moore arr i o) }

data Moore arr i o = forall x. Moore !x !(arr x o) !(Mealy arr i o)


type Mealy' = Mealy (->)
type MealyK m = Mealy (Kleisli m)
type Moore' = Moore (->)
type MooreK m = Moore (Kleisli m)


pattern Mealy' m = Mealy m :: Mealy' i o
pattern MealyK m = Mealy m :: MealyK m i o

pattern Moore' x y z = Moore x y z :: Moore' i o
pattern MooreK x y z = Moore x y z :: MooreK m i o


mealy = Mealy
moore o = Moore o id


simple :: Arrow arr => arr i o -> Mealy arr i o
simple f = m
  where
    m = mealy $ arr (flip moore m) . f


lower :: ArrowApply arr => arr (Mealy arr a b) (arr a b)
lower = proc  m -> do
  m' <- arr runMealy -< m
  returnA -< m' >>> poop


-- -- liftArr :: Profunctor arr => arr a b -> Mealy arr a b
-- -- liftArr p = r where r = Mealy $ rmap (\b' -> Moore . (b',) $ r) p
-- --
-- --
-- -- lowerA :: Profunctor arr => Mealy arr a b -> arr a b
-- -- lowerA (Mealy f) = rmap copoint f
-- --


poop :: ArrowApply arr => arr (Moore arr i o) o
poop = proc m -> do
  Moore x fx _ <- id -< m
  app -< (fx, x)


chomp :: Moore arr i o -> Mealy arr i o
chomp (Moore _ _ m) = m


feedback :: ArrowApply arr => arr (Moore arr (o, i) o) (Moore arr i o)
feedback = proc m -> do
  Moore x fx m' <- id -< m
  o <- app -< (fx, x)
  returnA -< Moore o id $ feedback' m' o


feedback' :: ArrowApply arr => Mealy arr (o, i) o -> o -> Mealy arr i o
feedback' (Mealy m) o = Mealy $ proc i -> do
  m' <- m -< (o, i)
  feedback -< m'


instance Profunctor arr => Profunctor (Mealy arr) where
  dimap f g (Mealy m) = Mealy $ dimap f (dimap f g) m

instance Profunctor arr => Profunctor (Moore arr) where
  dimap f g (Moore x fx m) = Moore x (rmap g fx) (dimap f g m)


instance Profunctor arr => Functor (Mealy arr i) where
  fmap = rmap

instance Profunctor arr => Functor (Moore arr i) where
  fmap = rmap

-- TODO
-- do I really need ArrowApply?!
instance ArrowApply arr => Category (Mealy arr) where
  id = r where r = Mealy . arr $ \i -> Moore i id r

  Mealy f . Mealy g = r
    where
      r = Mealy $ proc i -> do
            Moore x fx m <- g -< i
            i' <- app -< (fx, x)
            Moore x' fx' m' <- f -< i'
            returnA -< Moore x' fx' (m' . m)


instance ArrowApply arr => Arrow (Mealy arr) where
  arr f = r where r = Mealy . arr $ \i -> Moore i (arr f) r

  first (Mealy m) = Mealy $ proc (i, d) -> do
    Moore x fx m' <- m -< i
    returnA -< Moore (x, d) (first fx) (first m')


instance (Profunctor arr, Arrow arr) => Applicative (Mealy arr i) where
  pure o = Mealy . arr $ \_ -> pure o

  Mealy mf <*> Mealy mx =
    Mealy $ proc i -> do
      mx' <- mx -< i
      mf' <- mf -< i
      returnA -< mf' <*> mx'


instance (Profunctor arr, Arrow arr) => Applicative (Moore arr i) where
  pure o = Moore o id (pure o)

  Moore x fx m <*> Moore x' fx' m' = Moore (x, x') g (m <*> m')
    where g = proc (y, y') -> do
                fa <- fx -< y
                a <- fx' -< y'
                returnA -< fa a


instance ArrowApply arr => ArrowChoice (Mealy arr) where
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)


instance ArrowApply arr => Thread arr (Mealy arr) where
  thread f f' (Mealy m) = Mealy (f >>> m')
    where
      m' = proc i -> do
            Moore x fx m'' <- m -< i
            o' <- f' <<< app -< (fx, x)
            returnA -< moore o' $ thread f f' m''


instance ArrowApply arr => Thread arr (Moore arr) where
  thread f f' (Moore x fx m) = Moore x (fx >>> f') $ thread f f' m


hoistMealy
  :: Arrow arr
  => (forall a b. arr a b -> arr' a b) -> Mealy arr i o -> Mealy arr' i o
hoistMealy f (Mealy m) = Mealy (f $ m >>> arr (hoistMoore f))


hoistMoore
  :: Arrow arr
  => (forall a b. arr a b -> arr' a b) -> Moore arr i o -> Moore arr' i o
hoistMoore f (Moore x fx m) = Moore x (f fx) $ hoistMealy f m


generalize :: Arrow arr => Mealy' i o -> Mealy arr i o
generalize = hoistMealy arr


generalize' :: Arrow arr => Moore' i o -> Moore arr i o
generalize' = hoistMoore arr


simplify :: MealyK Identity i o -> Mealy' i o
simplify = hoistMealy $ runKleisli >>> fmap runIdentity


simplify' :: MooreK Identity i o -> Moore' i o
simplify' = hoistMoore $ runKleisli >>> fmap runIdentity



-- -- -- Foldable is special to (->) and Kleisli somehow? -- -- -- somehow this is particular to Hask -- --
-- -- -- I think this boilerplate could be reduces.
-- -- -- we see this idea of e.g. chompsF being applied recursively.
-- -- -- surely this is happening other places?
-- -- -- is it related to feedback somehow?
-- --
-- --
-- -- chompsF :: Foldable f => Moore' a b -> Moore' (f a) b
-- -- chompsF m@(Moore (b, _)) = Moore (b, Mealy f)
-- --   where
-- --     f xs = chompsF $ foldl (\g x -> feed (neglect g) x) m xs
-- --
-- --
-- -- chompsK :: (Foldable f, Monad m) => MooreK m i o -> MooreK m (f i) o
-- -- chompsK m@(Moore (b, _)) = Moore (b, Mealy $ Kleisli f)
-- --   where
-- --     f xs = chompsK <$> foldlM (\g x -> feedK (neglect g) x) m xs
-- --
-- --
-- --
-- --
-- --
-- --
-- --


-- foldMoore :: Foldable f => Moore a b -> Moore (f a) b
-- foldMoore m@(Moore' x fx fi) = Moore' x fx . rmap foldMoore $ foldl feedF m
--
--
-- foldMooreK :: (Foldable f, Monad m) => MooreK m a b -> MooreK m (f a) b
-- foldMooreK m@(Moore' x fx fi) = Moore' x fx . rmap foldMooreK $ Kleisli (foldlM feedK m)
