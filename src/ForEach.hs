module ForEach
  ( module ForEach.Prelude
  , enumFromTo
  , sum
  , map
  , filter
  , length
  , take
  , replicate
  ) where

import ForEach.Prelude
import Prelude (Num (..), Ord (..), Monad (..), ($), ($!), undefined, Bool, Int, fmap)

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
sum = forEach 0 $ \total x c ->
  continue_ c $! total + x
{-# INLINE sum #-}

map
  :: Monad m
  => (a -> b)
  -> Stream m a
  -> Stream m b
map = fmap
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

length
  :: Monad m
  => Stream m a
  -> m Int
length = forEach 0 $ \total _ c -> continue_ c (total + 1)
{-# INLINE length #-}

take
  :: Monad m
  => Int
  -> Stream m a
  -> Stream m a
-- FIXME this forces one extra value to be pulled in
take count0 = forEachG count0 $ \count a g ->
  if count <= 0
    then done_ g
    else yield_ g (count - 1) a
{-# INLINE take #-}

replicate
  :: Monad m
  => Int
  -> a
  -> Stream m a
replicate count0 a = generate count0 $ \count g ->
  if count <= 0
    then done_ g
    else yield_ g (count - 1) a
{-# INLINE replicate #-}
