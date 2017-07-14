import ForEach.Prelude
import qualified ForEach as FE
import Criterion.Main
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified RawStream as RS
import System.IO.Unsafe

main :: IO ()
main = defaultMain
  [ bgroup "enum+filter+map+sum"
    [ bench "RawStream/foreach mix" $ whnf
       (\high -> runIdentity $ RS.sum $ RS.map (* 2) $ RS.filter even $ FE.enumFromTo 1 high)
       high'
    , bench "RawStream" $ whnf
       (\high -> runIdentity $ RS.sum $ RS.map (* 2) $ RS.filter even $ RS.enumFromTo 1 high)
       high'
    , bench "RawStream IO" $ whnfIO
       $ RS.sum $ RS.map (* 2) $ RS.filter even $ RS.enumFromTo 1 high'
    , bench "foreach" $ whnf
       (\high -> runIdentity $ FE.sum $ FE.map (* 2) $ FE.filter even $ FE.enumFromTo 1 high)
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
  ]
  where
    high' :: Int
    high' = 1000000
