-- -*- haskell-hugs-program-args: ("+." "-98") -*-

-- A monad of mutable circular lists.

module Control.Monad.Adaptive.CircularList(
  CircularList,
  circularList,
  val,
  update,
  next,
  previous,
  insert,
  delete) where

import Control.Monad.Adaptive.Ref

-- Export:
circularList :: Ref m r => a -> m (CircularList m r a)
val          :: Ref m r => CircularList m r a -> m a
next         :: Ref m r => CircularList m r a -> m (CircularList m r a)
update       :: Ref m r => CircularList m r a -> a -> m ()
previous     :: Ref m r => CircularList m r a -> m (CircularList m r a)
insert       :: Ref m r => CircularList m r a -> a -> m (CircularList m r a)
delete       :: Ref m r => CircularList m r a -> m ()

-- Local:

data CircularList m r a = CL (r (CircularList m r a,a,CircularList m r a))
                        | DummyCL (m a)

deCL (CL l) = l

circularList a = do
  r <- newRef undefined
  let l = CL r
  writeRef r (l,a,l)
  return l

get :: Ref m r => CircularList m r a -> 
                  m (CircularList m r a, a,CircularList m r a)
get = readRef . deCL

set :: Ref m r => CircularList m r a -> 
                  (CircularList m r a, a,CircularList m r a) -> m ()
set = writeRef . deCL

update l a = do
         (p,_,n) <- get l
         set l (p,a,n)

val l = (\ (p,a,n) -> a) `fmap` get l

next l = (\ (p,a,n) -> n) `fmap` get l

previous l = (\ (p,a,n) -> p) `fmap` get l

insert l a = do
  (p,b,n) <- get l
  n' <- CL `fmap` newRef (l,a,n)
  set l (p,b,n')
  nl <- next n'
  (_,nb,nn) <- get nl
  set nl (n',nb,nn)
  return n'


delete l = do
  (p,_,n) <- get l
  (pp,a,_) <- get p
  set p (pp,a,n)
  (_,a',nn) <- get n
  set n (p,a',nn)
