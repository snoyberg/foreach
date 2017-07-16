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
import Prelude (Num (..), Ord (..), Monad (..), ($), ($!), undefined, Bool, Int, fmap, otherwise)

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
filter pred = transform () $ \next s () g -> do
  step <- next s
  case step of
    Done -> done_ g
    Skip s' -> skip_ g (s', ())
    Yield s' a
      | pred a -> yield_ g (s', ()) a
      | otherwise -> skip_ g (s', ())
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
take count0 = transform count0 $ \next s count g ->
  if count <= 0
    then done_ g
    else do
      step <- next s
      case step of
        Done -> done_ g
        Skip s' -> skip_ g (s', count)
        Yield s' a -> yield_ g (s', count - 1) a
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
