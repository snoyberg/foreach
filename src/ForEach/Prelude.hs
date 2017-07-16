{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ForEach.Prelude
  ( Stream
  , Step (..)
  , Generator
  , Identity (..)
  , done_
  , skip_
  , yield_
  , generate
  , Consumer
  , continue_
  , break_
  , forEach
  , transform
  ) where

import ForEach.Internal
import Data.Functor.Identity

data Generator s a m final = Generator
  { done' :: m final
  , skip' :: s -> m final
  , yield' :: s -> a -> m final
  }

done_ :: Generator s a m final -> m final
done_ = done'

skip_ :: Generator s a m final -> s -> m final
skip_ = skip'

yield_ :: Generator s a m final -> s -> a -> m final
yield_ = yield'

generate
  :: forall m s a.
     Monad m
  => s -- ^ initial state
  -> (forall final. s -> Generator s a m final -> m final)
  -> Stream m a
generate state0 func = Stream state0 $ \s -> func s generator
{-# INLINE generate #-}

generator :: Monad m => Generator s a m (Step s a)
generator = Generator
  { done' = return Done
  , skip' = return . Skip
  , yield' = \s a -> return $ Yield s a
  }

data Consumer accum m final = Consumer
  { continue' :: accum -> m final
  , break' :: accum -> m final
  }

continue_ :: Consumer accum m final -> accum -> m final
continue_ = continue'

break_ :: Consumer accum m final -> accum -> m final
break_ = break'

forEach
  :: forall accum m final a.
     Monad m
  => accum -- ^ initial accumulator
  -> (forall final. accum -> a -> Consumer accum m final -> m final)
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
          res <- func accum a consumer
          case res of
            Left accum' -> return accum'
            Right accum' -> loop accum' s'
{-# INLINE forEach #-}

consumer :: Monad m => Consumer accum m (Either accum accum)
consumer = Consumer
  { continue' = return . Right
  , break' = return . Left
  }

transform
  :: forall bstate m final a b.
     Monad m
  => bstate -- ^ initial state
  -> (forall astate final.
      (astate -> m (Step astate a)) ->
      astate ->
      bstate ->
      Generator (astate, bstate) b m final ->
      m final)
  -> Stream m a
  -> Stream m b
transform bstate0 func (Stream astate0 next) =
  let func' = func next
   in Stream (astate0, bstate0) $ \(astate, bstate) ->
        func' astate bstate generator
{-# INLINE transform #-}
