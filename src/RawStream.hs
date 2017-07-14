{-# LANGUAGE BangPatterns #-}
module RawStream where

import ForEach.Internal
import Prelude hiding (filter, map, sum)

enumFromTo :: (Monad m, Num a, Ord a) => a -> a -> Stream m a
enumFromTo low high = Stream low $ \curr ->
  return $ if curr > high
    then Done
    else Yield (curr + 1) curr
{-# INLINE enumFromTo #-}

filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter p (Stream s0 next) = Stream s0 $ \s -> do
  step <- next s
  return $ case step of
    Done -> Done
    Skip s' -> Skip s'
    Yield s' a
      | p a -> Yield s' a
      | otherwise -> Skip s'
{-# INLINE filter #-}

map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f (Stream s0 next) = Stream s0 $ \s -> do
  step <- next s
  return $ case step of
    Done -> Done
    Skip s' -> Skip s'
    Yield s' a -> Yield s' (f a)
{-# INLINE map #-}

sum :: (Num a, Monad m) => Stream m a -> m a
sum (Stream s0 next) =
  loop s0 0
  where
    loop s !total = do
      step <- next s
      case step of
        Done -> return total
        Skip s' -> loop s' total
        Yield s' a -> loop s' (total + a)
{-# INLINE sum #-}
