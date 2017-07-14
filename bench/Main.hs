{-# LANGUAGE OverloadedLists #-}
import ForEach.Prelude
import qualified ForEach as FE
import Criterion.Main
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe
import Data.Foldable (foldl')

main :: IO ()
main = defaultMain
  [ bgroup "enum+filter+map+sum"
    [ bench "foreach" $ whnf
       (\high -> runIdentity $ FE.sum $ FE.map (* 2) $ FE.filter even $ FE.enumFromTo 1 high)
       high'
    , bench "foreach overloaded lists" $ whnf
       (\high -> runIdentity $ FE.sum $ FE.map (* 2) $ FE.filter even [1..high])
       high'
    , bench "foreach foldl'" $ whnf
       (\high -> foldl' (+) 0 $ FE.map (* 2) $ FE.filter even $ FE.enumFromTo 1 high)
       high'
    , bench "foreach foldable sum" $ whnf
       (\high -> sum $ FE.map (* 2) $ FE.filter even $ FE.enumFromTo 1 high)
       high'
    , bench "foreach IO" $ whnfIO
       $ FE.sum $ FE.map (* 2) $ FE.filter even $ FE.enumFromTo 1 high'
    , bench "list" $ whnf
       (\high -> sum $ map (* 2) $ filter even $ [1..high])
       high'
    , bench "boxed vector" $ whnf
       (\high -> VB.sum $ VB.map (* 2) $ VB.filter even $ VB.enumFromTo 1 high)
       high'
    , bench "unboxed vector" $ whnf
       (\high -> VU.sum $ VU.map (* 2) $ VU.filter even $ VU.enumFromTo 1 high)
       high'
    ]
  , bgroup "simple sum"
    [ bench "foreach" $ whnf
      (\high -> runIdentity $ FE.sum $ FE.enumFromTo 1 high)
      high'
    , bench "list" $ whnf
      (\high -> sum ([1..high] :: [Int]))
      high'
    , bench "boxed vector" $ whnf
      (\high -> VB.sum $ VB.enumFromTo 1 high)
      high'
    , bench "unboxed vector" $ whnf
      (\high -> VU.sum $ VU.enumFromTo 1 high)
      high'
    ]
  , bgroup "replicate+take+length"
    [ bench "foreach" $ whnf
      (\count -> runIdentity $ FE.length $ FE.take count $ FE.replicate (count * 2) ())
      count'
    , bench "list" $ whnf
      (\count -> length $ take count $ replicate (count * 2) ())
      count'
    , bench "boxed vector" $ whnf
      (\count -> VB.length $ VB.take count $ VB.replicate (count * 2) ())
      count'
    , bench "unboxed vector" $ whnf
      (\count -> VU.length $ VU.take count $ VU.replicate (count * 2) ())
      count'
    ]
  , bgroup "enum+length"
    [ bench "foreach" $ whnf
      (\high -> runIdentity $ FE.length $ FE.enumFromTo 1 high)
      high'
    , bench "list" $ whnf
      (\high -> length ([1..high] :: [Int]))
      high'
    , bench "boxed vector" $ whnf
      (\high -> VB.length $ VB.enumFromTo 1 high)
      high'
    , bench "unboxed vector" $ whnf
      (\high -> VU.length $ VU.enumFromTo 1 high)
      high'
    ]
  ]
  where
    high' :: Int
    high' = 1000000

    count' :: Int
    count' = 1000000
