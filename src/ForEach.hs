module ForEach
  ( module ForEach.Prelude
  , enumFromTo
  , sum
  , map
  , filter
  ) where

import ForEach.Prelude
import Prelude (Num (..), Ord (..), Monad (..), ($), ($!), undefined, Bool, Int)

enumFromTo
  :: (Num a, Ord a, Monad m)
  => a
  -> a
  -> Stream m a
enumFromTo low high = generate low $ \curr g ->
  if curr >= high
    then done_ g
    else yield_ g (curr + 1) curr
{-# INLINE enumFromTo #-}

sum
  :: (Num a, Monad m)
  => Stream m a
  -> m a
sum stream = forEach 0 stream $ \total x c ->
  continue_ c $! total + x
{-# INLINE sum #-}

map
  :: Monad m
  => (a -> b)
  -> Stream m a
  -> Stream m b
map f = forEachG () $ \() a g -> yield_ g () (f a)
{-# INLINE map #-}

filter
  :: Monad m
  => (a -> Bool)
  -> Stream m a
  -> Stream m a
filter pred = forEachG () $ \() a g ->
  if pred a
    then yield_ g () a
    else skip_ g ()
{-# INLINE filter #-}
