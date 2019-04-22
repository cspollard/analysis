{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Mealy where


import           Control.Arrow
import           Control.Category
import           Data.Bifunctor   (bimap)
import           Data.Profunctor
import           Prelude          hiding (id, (.))


newtype Mealy p a b = Mealy { runMealy :: p a (Mealy p a b, b) }

instance Profunctor p => Profunctor (Mealy p) where
  dimap f g (Mealy m) = Mealy $ dimap f (bimap (dimap f g) g) m

instance Profunctor p => Functor (Mealy p a) where
  fmap f (Mealy g) = Mealy $ rmap (bimap (fmap f) f) g


instance (Profunctor p, Arrow p) => Applicative (Mealy p a) where
  pure b = r where r = Mealy $ arr (const (r, b))

  Mealy mf <*> Mealy mz = Mealy $ proc x -> do
    (mz', z) <- mz -< x
    (mf', f) <- mf -< x
    returnA -< (mf' <*> mz', f z)


fold :: Arrow p => p (b, a) b -> b -> Mealy p a b
fold f b = m b
  where
    m x = Mealy $ proc a -> do
      x' <- f -< (x, a)
      returnA -< (m x', x')




-- how do we extract?
-- how do we run over many inputs?

-- does this really need Arrow?
-- seems to need at least Strong, which is ~ Arrow, right?
instance Arrow p => Category (Mealy p) where
  id = r where r = Mealy . arr $ \a -> (r, a)

  Mealy myz . Mealy mxy =
    Mealy $ proc x -> do
      (mxy', y) <- mxy -< x
      (myz', z) <- myz -< y
      returnA -< (myz' . mxy', z)


instance Arrow p => Arrow (Mealy p) where
  arr f =
    Mealy $ proc x -> do
      y <- arr f -< x
      returnA -< (arr f, y)

  first (Mealy f) =
    Mealy $ proc (a, c) -> do
      (m', b) <- f -< a
      returnA -< (first m', (b, c))

-- instance (Profunctor p, Arrow p) => Monad (Mealy p a) where
--   return = pure
--
--   -- mx :: Mealy p x y
--   -- f :: y -> Mealy p x z
--   mxy >>= f = Mealy $ proc x -> do
--     (_mxy', y) <- mxy -< x
--
--     let Mealy mxz = f y
--
--     (mxz', z) <- mxz -< x
--     returnA -< (mxz', z)
