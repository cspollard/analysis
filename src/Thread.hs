{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Thread where

import           Data.Bifunctor
import           Data.Copointed
import           Data.Pointed
import           Data.Profunctor


newtype Thread p q a b = Thread { unthread :: p a (q a b) }

newtype Unthread p q a b = Unthread { thread :: q (p a b) b }

type Mealy = Thread (->) (,)
type Moore = Unthread (->) (,)


instance (Profunctor p, Profunctor q) => Profunctor (Thread p q) where
  dimap f g (Thread p) = Thread $ dimap f (dimap f g) p

instance (Profunctor p, Bifunctor q) => Profunctor (Unthread p q) where
  dimap f g (Unthread q) = Unthread $ bimap (dimap f g) g q


instance (Profunctor p, Profunctor q) => Functor (Thread p q a) where
  fmap = rmap

instance (Profunctor p, Bifunctor q) => Functor (Unthread p q a) where
  fmap = rmap


class Strong p q where
  flex :: p a b -> p (q a c) (q b c)


-- instance (Category p) => Category (Thread p q a) where
--
--   id = Thread r where r = -- p a (q a a)
--     Thread
--     $ id -- p a (Unthread p q a a)
--     .
--
-- -- Pointed
-- -- Copointed
-- -- Applicative
