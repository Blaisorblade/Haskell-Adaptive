{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- A class for monads with mutable references

module Control.Monad.Adaptive.Ref where

import Control.Monad.ST
import Data.IORef
import Data.STRef

class EqRef r where
    eqRef :: r a -> r a -> Bool

class (EqRef r, Functor m, Monad m) => Ref m r | m -> r where
  newRef   :: a -> m (r a)
  readRef  :: r a -> m a
  writeRef :: r a -> a -> m ()

instance EqRef (STRef s) where eqRef = (==)

instance Ref (ST s) (STRef s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance EqRef IORef where eqRef = (==)

instance Ref IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

mapRef :: Ref m r => (a -> a) -> r a -> m ()
mapRef f r = readRef r >>= writeRef r . f
