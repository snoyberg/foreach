{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module ForEach.Internal where

import Data.Functor.Identity
import Data.Monoid

data Step s a
  = Done
  | Skip s
  | Yield s a
  deriving Functor

data Stream m a = forall s. Stream s (s -> m (Step s a))
deriving instance Functor m => Functor (Stream m)

instance m ~ Identity => Foldable (Stream m) where
  foldr f accum0 (Stream s0 next) =
    runIdentity $ loop s0 accum0
    where
      loop s accum = do
        step <- next s
        case step of
          Done -> return accum
          Skip s' -> loop s' accum
          Yield s' a -> loop s' (f a accum)
  {-# INLINE foldr #-}

  foldl f accum0 (Stream s0 next) =
    runIdentity $ loop s0 accum0
    where
      loop s !accum = do
        step <- next s
        case step of
          Done -> return accum
          Skip s' -> loop s' accum
          Yield s' a -> loop s' (f accum a)
  {-# INLINE foldl #-}

  foldMap f (Stream s0 next) =
    runIdentity $ loop s0 mempty
    where
      loop s accum = do
        step <- next s
        case step of
          Done -> return accum
          Skip s' -> loop s' accum
          Yield s' a -> loop s' (accum <> f a)
  {-# INLINE foldMap #-}
