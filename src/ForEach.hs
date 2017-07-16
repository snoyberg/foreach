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
enumFromTo low high = generate low $ \curr ->
  return $
  if curr >= high
    then Done
    else Yield (curr + 1) curr
{-# INLINE enumFromTo #-}

sum
  :: (Num a, Monad m)
  => Stream m a
  -> m a
sum = forEach 0 $ \total x -> return $ Continue $! total + x
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
filter pred = transform () $ \next s () -> do
  step <- next s
  return $ case step of
    Done -> Done
    Skip s' -> Skip (s', ())
    Yield s' a
      | pred a -> Yield (s', ()) a
      | otherwise -> Skip (s', ())
{-# INLINE filter #-}

length
  :: Monad m
  => Stream m a
  -> m Int
length = forEach 0 $ \total _ -> return $ Continue $! total + 1
{-# INLINE length #-}

take
  :: Monad m
  => Int
  -> Stream m a
  -> Stream m a
take count0 = transform count0 $ \next s count ->
  if count <= 0
    then return Done
    else do
      step <- next s
      return $ case step of
        Done -> Done
        Skip s' -> Skip (s', count)
        Yield s' a -> Yield (s', count - 1) a
{-# INLINE take #-}

replicate
  :: Monad m
  => Int
  -> a
  -> Stream m a
replicate count0 a = generate count0 $ \count ->
  return $
  if count <= 0
    then Done
    else Yield (count - 1) a
{-# INLINE replicate #-}
