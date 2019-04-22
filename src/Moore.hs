{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module Moore
  ( Moore'(..)
  , Moore, pattern Moore, MooreK, pattern MooreK
  , moore, moore'
  , feed, feedF, feedK
  , chompF, chompK
  , poop, poopF, poopK
  , feedback
  , Kleisli(..)
  , hoistMoore, generalize, simplify
  , Thread(..)
  , (|>>), (|<<), (>>|), (<<|)
  ) where

import           Control.Arrow
import           Control.Category
import           Data.Bifunctor
import           Data.Bifunctor.Biff
import           Data.Bifunctor.Join
import           Data.Copointed
import           Data.Functor.Identity
import           Data.Pointed
import           Data.Profunctor
import           Prelude               hiding (id, (.))

-- why not alternate Mealy and Moore'?


data Moore' arr i o = forall x. Moore' !x !(arr x o) !(arr i (Moore' arr i o))

type Moore = Moore' (->)
type MooreK m = Moore' (Kleisli m)

pattern Moore x y z = Moore' x y z :: Moore i o
pattern MooreK x y z = Moore' x y z :: MooreK m i o


moore :: Arrow arr => arr i o -> i -> Moore' arr i o
moore f i = Moore' i f $ arr (moore f)

moore' :: Arrow arr => arr i o -> o -> Moore' arr i o
moore' f o = Moore' o id $ arr (moore f)

feed :: Moore' arr i o -> arr i (Moore' arr i o)
feed (Moore' _ _ fi) = fi

feedF :: Moore i o -> i -> Moore i o
feedF = feed

feedK :: MooreK m i o -> i -> m (MooreK m i o)
feedK = runKleisli . feed


chompF :: i -> Moore i o -> Moore i o
chompF = flip feed

chompK :: i -> MooreK m i o -> m (MooreK m i o)
chompK = flip feedK

poop :: (forall x. arr x o -> x -> c) -> Moore' arr i o -> c
poop run (Moore' x fx _) = run fx x

poopF :: Moore i o -> o
poopF = poop ($)

poopK :: Monad m => MooreK m i o -> m o
poopK = poop runKleisli


feedback :: Arrow arr => Moore' arr (o, i) o -> Moore' arr i o
feedback (Moore' x fx fio) =
  Moore' x fx $ proc i -> do
    o <- fx -< x
    mi' <- fio -< (o, i)
    returnA -< feedback mi'


hoistMoore
  :: (Profunctor arr, Profunctor arr')
  => (forall a b. arr a b -> arr' a b) -> Moore' arr i o -> Moore' arr' i o
hoistMoore f (Moore' x fx fi) = Moore' x (f fx) (rmap (hoistMoore f) (f fi))


generalize :: (Profunctor arr, Arrow arr) => Moore i o -> Moore' arr i o
generalize = hoistMoore arr

simplify :: MooreK Identity i o -> Moore i o
simplify = hoistMoore (fmap runIdentity . runKleisli)


class Thread arr m | m -> arr where
  thread :: arr i' i -> arr o o' -> m i o -> m i' o'

instance Thread (->) (->) where
  thread f g h = f >>> h >>> g

instance Monad m => Thread (Kleisli m) (Kleisli m) where
  thread f g h = f >>> h >>> g

instance Arrow arr => Thread arr (Moore' arr) where
  thread f g (Moore' x fx fi) =
    Moore' x (fx >>> g) (f >>> fi >>> arr (thread f g))


infixr 1 >>|
(>>|) :: (Category arr, Thread arr m) => arr i' i -> m i o -> m i' o
f >>| m = thread f id m

infixr 1 |<<
(|<<) :: (Category arr, Thread arr m) => m i o -> arr i' i -> m i' o
(|<<) = flip (>>|)

infixr 1 |>>
(|>>) :: (Category arr, Thread arr m) => m i o -> arr o o' -> m i o'
m |>> f = thread id f m

infixr 1 <<|
(<<|) :: (Category arr, Thread arr m) => arr o o' -> m i o -> m i o'
(<<|) = flip (|>>)

instance Profunctor arr => Profunctor (Moore' arr) where
  dimap f g (Moore' x fx fi) = Moore' x (rmap g fx) (dimap f (dimap f g) fi)

instance Profunctor arr => Functor (Moore' arr a) where
  fmap = rmap

instance (Profunctor arr, Arrow arr) => Applicative (Moore' arr a) where
  pure b = r where r = Moore' b id $ arr (const r)

  Moore' f gf gi <*> Moore' x hx hi = Moore' x hx' hi'
    where
      hx' = proc x' -> do
        fab <- gf -< f
        a <- hx -< x'
        returnA -< fab a

      hi' = proc i -> do
        (mfab, ma) <- (gi &&& hi) -< i
        returnA -< mfab <*> ma



-- instance (Profunctor p, Arrow p) => Pointed (Moore' p a) where
--   point = pure
--
--
-- instance Copointed (Moore' p a) where
--   copoint (Moore' (b, _)) = b
--
--
-- -- instance (Category p, Profunctor p) => Comonad (Moore' p a) where
-- --   extract (Moore' (b, _)) = b
-- --   extend f w@(Moore' (_, g)) = Moore' (f w, extend f . g)
-- --
-- --
-- -- folding :: Arrow p => p (b, a) b -> b -> Moore' p a b
-- -- folding f b = Moore' (b, go b f)
-- --   where
-- --     go :: Arrow p => c -> p (c, a) c -> p a (Moore' p a c)
-- --     go c g = proc a -> do
-- --       c' <- g -< (c, a)
-- --       returnA -< Moore' (c', go c' g)
-- --
-- --
-- -- liftA :: Profunctor p => p a b -> b -> Moore' p a b
-- -- liftA p b = Moore' (b, rmap (\b' -> liftA p b') p)
-- --
-- -- extract :: Moore' p a b -> b
-- -- extract (Moore' (b, _)) = b
-- --
-- -- feed :: Moore' p a b -> p a (Moore' p a b)
-- -- feed (Moore' (_, f)) = f
-- --
-- --
-- -- runF :: Moore' (->) a b -> a -> b
-- -- runF (Moore' (_, f)) a = fst . runMoore' $ (f $ a)
-- --
-- --
-- -- runM :: Functor m => Moore' (Kleisli m) a b -> a -> m b
-- -- runM (Moore' (_, k)) a = fmap (fst . runMoore') $ k `runKleisli` a
