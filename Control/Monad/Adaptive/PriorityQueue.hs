
-- A naive priority queue implementation, with an insert operation
-- that uses a monadic comparison operation.

module Control.Monad.Adaptive.PriorityQueue(
  PriorityQueue,
  empty,
  insert,
  insertM,
  min
  ) where

import Prelude hiding(min)

import qualified List(insert)
import Monad(ap)

-- Export:
empty   :: PriorityQueue a
insert  :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertM :: Monad m => 
          (a -> a -> m Ordering) -> a -> PriorityQueue a -> m (PriorityQueue a)
min     :: PriorityQueue a -> Maybe (a, PriorityQueue a)

-- Local

newtype PriorityQueue a = PQ [a]

empty = PQ []

insert a (PQ l) = PQ (List.insert a l)


insertM cmp a (PQ l) = return PQ `ap` ins l
  where ins [] = return [a]
        ins (b:l) = do o <- cmp a b
                       case o of LT -> return (a:b:l)
                                 _  -> return (b:) `ap` ins l

min (PQ []) = Nothing
min (PQ (x:xs)) = Just (x,PQ xs)
