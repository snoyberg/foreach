{-# LANGUAGE ExistentialQuantification #-}
module ForEach.Internal where

data Step s a
  = Done
  | Skip s
  | Yield s a

data Stream m a = forall s. Stream s (s -> m (Step s a))
