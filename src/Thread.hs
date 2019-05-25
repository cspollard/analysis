{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Thread where

import           Control.Arrow
import           Control.Category
import           Prelude          hiding (id, (.))

class Thread arr m | m -> arr where
  thread :: arr i' i -> arr o o' -> m i o -> m i' o'

instance Thread (->) (->) where
  thread f g h = f >>> h >>> g

instance Monad m => Thread (Kleisli m) (Kleisli m) where
  thread f g h = f >>> h >>> g


infixr 2 >>|
(>>|) :: (Category arr, Thread arr m) => arr i' i -> m i o -> m i' o
f >>| m = thread f id m

infixr 2 |<<
(|<<) :: (Category arr, Thread arr m) => m i o -> arr i' i -> m i' o
(|<<) = flip (>>|)

infixr 2 |>>
(|>>) :: (Category arr, Thread arr m) => m i o -> arr o o' -> m i o'
m |>> f = thread id f m

infixr 2 <<|
(<<|) :: (Category arr, Thread arr m) => arr o o' -> m i o -> m i o'
(<<|) = flip (|>>)
