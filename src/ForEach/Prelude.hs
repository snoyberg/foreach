{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ForEach.Prelude
  ( Stream
  , Step (..)
  , Loop (..)
  , Identity (..)
  , generate
  , forEach
  , transform
  ) where

import ForEach.Internal
import Data.Functor.Identity

generate
  :: forall m s a.
     Monad m
  => s -- ^ initial state
  -> (s -> m (Step s a))
  -> Stream m a
generate = Stream
{-# INLINE generate #-}

data Loop accum
  = Continue accum
  | Break accum

forEach
  :: forall accum m final a.
     Monad m
  => accum -- ^ initial accumulator
  -> (accum -> a -> m (Loop accum))
  -> Stream m a
  -> m accum
forEach accum0 func (Stream s0 next) =
    loop accum0 s0
  where
    loop accum s = do
      step <- next s
      case step of
        Done -> return accum
        Skip s' -> loop accum s'
        Yield s' a -> do
          res <- func accum a
          case res of
            Break accum' -> return accum'
            Continue accum' -> loop accum' s'
{-# INLINE forEach #-}

transform
  :: forall bstate m a b.
     Monad m
  => bstate -- ^ initial state
  -> (forall astate.
      (astate -> m (Step astate a)) ->
      astate ->
      bstate ->
      m (Step (astate, bstate) b))
  -> Stream m a
  -> Stream m b
transform bstate0 func (Stream astate0 next) =
  let func' = func next
   in Stream (astate0, bstate0) $ \(astate, bstate) ->
        func' astate bstate
{-# INLINE transform #-}
