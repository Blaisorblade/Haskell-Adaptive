{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- A monad for manipulating ordered lists.  Follows the implementation
-- given in the appendix of O'Neill's and Burton's JFP paper, but
-- doesn't impose any fixed limit of the number of elements.

-- References:

-- Dietz and Sleator: "Two algorithms for maintaining order in a
-- list", in Proc. of 19th ACM Symposium of Theory of Computing, 1987.

-- O'Neill and Burton: "A New Method For Functional Arrays", Journal
-- of Functional Programming, vol7, no 5, September 1997.

module Control.Monad.Adaptive.OrderedList(
  Record,
  OrderedList,
  rval,
  next,
  order,
  delete,
  spliceOut,
  deleted,
  insert,
  base,
  run,
  inM,
  record
  ) where

import Control.Monad(ap,unless)
import Control.Monad.Adaptive.MonadUtil
import Control.Monad.Adaptive.Ref
import Control.Monad.Adaptive.CircularList hiding (delete,insert,next,update)
import qualified Control.Monad.Adaptive.CircularList as CircularList

import System.IO.Unsafe(unsafePerformIO) -- for diagnostic

-- Export:
insert    :: Ref m r => Record m r a -> a -> OrderedList m r a (Record m r a)
next      :: Ref m r => Record m r a -> OrderedList m r a (Record m r a)
delete    :: Ref m r => Record m r a -> OrderedList m r a ()
spliceOut :: Ref m r => Record m r a -> Record m r a -> OrderedList m r a ()
deleted   :: Ref m r => Record m r a -> OrderedList m r a Bool
order     :: Ref m r => Record m r a -> Record m r a -> 
                        OrderedList m r a Ordering
rval      :: Ref m r => Record m r a -> OrderedList m r a a
run       :: Ref m r => OrderedList m r a b -> m b
inM       :: Ref m r => m b -> OrderedList m r a b
base      :: Ref m r => OrderedList m r a (Record m r a)


-- Local:

newtype Record m r a = Record (CircularList m r (Bool,Integer,a))
deR (Record r) = r

data OrderedList m r a b = OL ((r Integer,r Integer,Record m r a) -> m b)
deOL (OL f) = f

run l = do
    base <- Record `fmap` circularList (False,0,undefined)
    s <- newRef 0
    mr <- newRef m
    deOL l (mr,s,base)
  where 
    m = 2^16

inM m = OL $ \e -> m

instance Ref m r => Monad (OrderedList m r a) where
  return a = inM (return a)
  (OL m) >>= f = OL $ \e -> m e >>= \a -> deOL (f a) e

instance Ref m r => Functor (OrderedList m r a) where 
  fmap f m = m >>= return . f

instance Ref m r => Ref (OrderedList m r a) r where
  newRef v     = inM (newRef v)
  readRef r    = inM (readRef r)
  writeRef r v = inM (writeRef r v)

mop a o b = op2 o a b
op2 f a b = op1 f a `ap` b
op1 f a = return f `ap` a

instance Eq (OrderedList m r a b) where { }
instance Show (OrderedList m r a b) where { }

instance (Ref m r, Num b) => Num (OrderedList m r a b) where
  (+)         = op2 (+)
  (-)         = op2 (-)
  (*)         = op2 (*)
  negate      = op1 negate
  abs         = op1 abs
  signum      = op1 signum
  fromInteger = return . fromInteger
--  fromInt     = return . fromInt

instance Ord (OrderedList m r a b) where { }
instance (Ref m r, Real b) => Real (OrderedList m r a b) where { }
instance Enum (OrderedList m r a b) where { }

instance (Ref m r, Integral b) => Integral (OrderedList m r a b) where
  rem = op2 rem
  div = op2 div
  mod = op2 mod

base = OL $ \(m,n,b) -> return b

bigM :: Ref m r => OrderedList m r a Integer
bigM = OL $ \(m,n,b) -> readRef m

size :: Ref m r => OrderedList m r a Integer
size = OL $ \(m,n,b) -> readRef n

adjsize :: Ref m r => Integer -> OrderedList m r a ()
adjsize i = OL $ \(m,n,b) -> do s <- readRef n
                                writeRef n (s+i)

setSize :: Ref m r => Integer -> OrderedList m r a ()
setSize n' = OL $ \(m,n,b) -> writeRef n n'

record :: Ref m r => Record m r a -> OrderedList m r a (Bool,Integer,a)
record r = inM (val (deR r))

rval r = (\ (d,i,a) -> a) `fmap` record r

next r = Record `fmap` inM (CircularList.next (deR r))

s x = next x

-- label
l :: Ref m r => Record m r a -> OrderedList m r a Integer
l r = (\ (d,i,a) -> i) `fmap` record r

-- gap
g e f = (l f - l e) `mod` bigM

deleted r = (\ (d,i,a) -> d) `fmap` record r

lbase :: Ref m r => OrderedList m r a Integer
lbase = base >>= l

gstar :: Ref m r => Record m r a -> Record m r a -> OrderedList m r a Integer
gstar e f = ifM (mop (l e) (==) (l f))
             bigM
             (g e f)

order x y = do b <- base
               return (compare) `ap` g b x `ap` g b y



update :: Ref m r => ((Bool,Integer)->(Bool,Integer)) -> 
                     Record m r a -> OrderedList m r a ()
update f r = do
   (d,i,a) <- record r
   let (d',i') = f (d,i)
   inM (CircularList.update (deR r) (d',i',a))
   
delete r = unlessM (deleted r) $ do
             ifM (mop lbase (==) (l r))
               (error "OrderedList.delete on base element")
               (do inM (CircularList.delete (deR r))
                   update (\ (_,i) -> (True,i)) r
                   adjsize (-1)
                   checkinvariant)

spliceOut r s = next r >>= spl where
  spl r = do 
    unlessM (mop lbase (==) (l r)) $
        whenM ((==LT) `fmap` order r s)
              (do r' <- next r
                  delete r
                  spl r')

increaseBigM :: Ref m r => OrderedList m r a ()
increaseBigM = do OL $ \(m,n,b) -> mapRef (*2) m

insert r a = do
  ifM (deleted r) 
    (error "insert: deleted") $ do
    whenM (mop bigM (<=) (4*(size+1)*(size+1)))
      increaseBigM
    r' <- s r
    d <- gstar r r'
    unless (d > 1)
      (renumber r)
    li <- (l r + (gstar r r' `div` 2)) `mod` bigM
    inM (CircularList.insert (deR r) (False,li,a))
    adjsize 1
    checkinvariant
    next r

renumber :: Ref m r => Record m r a -> OrderedList m r a ()
renumber e = do
   let getj j e0 ej = do
          ifM (mop (g e0 ej) (>) (return (j * j)))
            (return (j,ej)) $ do
            ej' <- s ej
            ifM (mop (l ej') (==) (l e))
              (return (j,ej)) $ do
              getj (j+1) e0 ej'
   (j,sje) <- s e >>= getj 1 e
   d <- gstar e sje
   le <- l e
   m <- bigM
   let ren k ek | k == j     = return ()
                | otherwise  = do
          update (const (False,(le + ((k * d) `div` j)) `mod` m)) ek
          s ek >>= ren (k+1)
   s e >>= ren 1

checkinvariant :: Ref m r => OrderedList m r a ()
checkinvariant = return () -- prall >> base >>= inv
  where inv r = do
             r' <- s r
             unlessM (mop lbase (==) (l r')) $ do
               ifM (mop (order r r') (==) (return LT))
                   (inv r')
                   (error "invariant")


prall :: Ref m r => OrderedList m r a ()
prall = uprint "prall:" >> base >>= pr where
  pr r = do
    x <- l r
    uprint (show x)
    r' <- s r
    unlessM (mop (base >>= order r') (==) (return EQ))
        (pr r')

uprint s = OL$ (\s' -> unsafePerformIO (putStrLn s) `seq` return ())
