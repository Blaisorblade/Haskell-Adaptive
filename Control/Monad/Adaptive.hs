-- -*- haskell-hugs-program-args: ("+." "-98") -*-
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- An monadic variant of the library from "Adaptive Functional
-- Programming", by Acar, Blelloch and Harper (POPL 2002).

-- Magnus Carlsson, magnus@cse.ogi.edu

module Control.Monad.Adaptive
  ( Adaptive
  , Changeable
  , Modifiable
  , readMod
  , InM(..)
  , change
  , propagate
  , run
  , inCh
  , NewMod(..)
  , newMod

 ) where

import Prelude 
import Monad(ap,unless)
import Control.Monad.Adaptive.MonadUtil
import Control.Monad.Adaptive.Ref
import qualified Control.Monad.Adaptive.OrderedList as OL
import Control.Monad.Adaptive.OrderedList(OrderedList)
import qualified Control.Monad.Adaptive.PriorityQueue as PQ
import Control.Monad.Adaptive.PriorityQueue(PriorityQueue)

-- Export:
class InM m' where
   inM :: Ref m r => m a -> m' m r a

class (Monad (n m r), Ref m r) => NewMod n m r where
   newModBy :: (a -> a -> Bool) -> Changeable m r a -> n m r (Modifiable m r a)


newMod    :: (Eq a, NewMod n m r) => 
             Changeable m r a -> n m r (Modifiable m r a)
change    :: Ref m r => Modifiable m r a -> a -> Adaptive m r ()
propagate :: Ref m r => Adaptive m r ()
readMod   :: Ref m r => Modifiable m r a -> Changeable m r a
run       :: Ref m r => Adaptive m r a -> m a
inCh      :: Ref m r => Changeable m r a -> Adaptive m r a

-- Local:

type ReComp m r = (Adaptive m r (), TimeStamp m r, TimeStamp m r)
startTime (_,s,_) = s

type TimeStamp m r = OL.Record m r ()

newtype Adaptive m r a = 
  Ad ((r (PriorityQueue (ReComp m r)), r (TimeStamp m r)) -> 
      OrderedList m r () a)

newtype Changeable m r a = Ch (K (Adaptive m r ()) a)
type K b a = (a -> b) -> b

newtype Modifiable m r a = Mo (r a, r (a -> Adaptive m r ()), r [ReComp m r])

cont :: Ref m r => 
        ((a -> Adaptive m r ()) -> Adaptive m r ()) -> Changeable m r a
cont m = Ch m

deCh (Ch m) = m
deAd (Ad m) = m

inAd :: Ref m r => Adaptive m r a -> Changeable m r a
inAd m = Ch $ (m >>=)

class InOL m' where
  inOL :: Ref m r => OrderedList m r () b -> m' m r b

instance InOL Adaptive where
  inOL m = Ad $ const m

instance InOL Changeable where
  inOL m = inAd (inOL m)

instance Ref m r => Ref (Changeable m r) r where
  newRef v     = inM $ newRef v
  readRef x    = inM $ readRef x
  writeRef x v = inM $ writeRef x v

instance Ref m r => Monad (Changeable m r) where
  return a   = Ch $ \k -> k a
  Ch m >>= f = Ch $ \k -> m $ \a -> deCh (f a) k

instance Ref m r => Functor (Changeable m r) where
  fmap f m = m >>= return . f

instance Ref m r => Ref (Adaptive m r) r where
  newRef v     = inM $ newRef v
  readRef x    = inM $ readRef x
  writeRef x v = inM $ writeRef x v

instance Ref m r => Monad (Adaptive m r) where
  return a   = Ad $ \e -> return a
  Ad m >>= f = Ad $ \e -> m e >>= \a -> deAd (f a) e

instance Ref m r => Functor (Adaptive m r) where
  fmap f m = m >>= return . f

readMod (Mo (r,chg,es)) = do
   start <- inAd stepTime
   cont $ \k -> do
     let reader = do readRef r >>= k
                     now <- readCurrentTime
                     mapRef ((reader,start,now):) es
     reader

pqRef :: Ref m r => Adaptive m r (r (PriorityQueue (ReComp m r)))
pqRef = Ad $ \ (pq,ct) -> return pq

readPq :: Ref m r => Adaptive m r (PriorityQueue (ReComp m r))
readPq = pqRef >>= readRef
writePq a = pqRef >>= flip writeRef a

ctRef :: Ref m r => Adaptive m r (r (TimeStamp m r))
ctRef = Ad $ \ (pq,ct) -> return ct
readCurrentTime :: Ref m r => Adaptive m r (TimeStamp m r)
readCurrentTime = ctRef >>= readRef
writeCurrentTime a = ctRef >>= flip writeRef a

stepTime :: Ref m r => Adaptive m r (TimeStamp m r)
stepTime = do
    readCurrentTime >>= inOL . flip OL.insert () >>= writeCurrentTime
    readCurrentTime

instance InM Changeable where
  inM m = Ch $ (inM m >>=)

instance InM Adaptive where
  inM m = Ad $ const (OL.inM m)

change (Mo (r,changeR,es)) a = do
    chg <- readRef changeR
    chg a

propagate = do
   let prop = do
        pq <- readPq
        case PQ.min pq of
          Nothing -> return ()
          Just ((reader,start,stop),pq') -> do
            writePq pq'
            unlessM (inOL (OL.deleted start)) $ do
                inOL (OL.spliceOut start stop)
                writeCurrentTime start
                reader
            prop
   now <- readCurrentTime
   prop
   writeCurrentTime now


run m = OL.run $ do 
   pq  <- newRef PQ.empty
   ct  <- OL.base >>= newRef
   deAd m (pq,ct)

inCh (Ch m) = do
   x <- newRef (error "inCh")
   m (writeRef x)
   readRef x

instance EqRef r => Eq (Modifiable m r a) where
   (Mo (r1,_,_)) == (Mo (r2,_,_)) = eqRef r1 r2

newMod = newModBy (==)

instance Ref m r => NewMod Changeable m r where
  newModBy c ch = inAd $ newModBy c ch

insertPQ :: Ref m r => 
       r [ReComp m r] -> Adaptive m r ()
insertPQ esR = do
   es <- readRef esR
   pqR <- pqRef
   readRef pqR >>= ins es >>= writeRef pqR
  where
  ins []     pq = return pq
  ins (e:es) pq = PQ.insertM (\x y -> inOL $ 
                              OL.order (startTime x) (startTime y))
                             e pq >>= ins es

instance Ref m r => NewMod Adaptive m r where
  newModBy cmp c = do
  m <- newRef (error "newMod")
  changeR <- newRef (error "changeR")
  es <- newRef []
  let writeFirst v = do
        writeRef m v
        now <- stepTime
        writeRef changeR (writeAgain now)
      writeAgain t v = do
        v' <- readRef m
        unless (cmp v' v) $ do
          writeRef m v
          insertPQ es
          writeRef es []
        writeCurrentTime t
  writeRef changeR writeFirst
  inCh $ do
    v <- c
    write <- readRef changeR
    inAd $ write v
  return (Mo (m, changeR, es))
