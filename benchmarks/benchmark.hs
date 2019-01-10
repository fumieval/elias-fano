{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
import Gauge.Main

import Codec.EliasFano
import qualified Data.Binary as B
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV
import Data.Word (Word64)
import qualified Data.IntSet as IS
import System.Random

td :: V.Vector Word64
td = V.scanl (+) 0 $ V.fromList $ map (toEnum . fromEnum) $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

main = do
  let ef = unsafeFromVector td
  let gen = V.scanl (+) 0 <$> V.replicateM 10000 (randomRIO (0, 256))
  td1 <- gen
  td2 <- gen
  defaultMain
    [ bench "encode/elias-fano" $ whnf unsafeFromVector td
    , bench "reverse/vector" $ whnf V.reverse td
    , bench "access/elias-fano" $ nf (map (ef!)) [0..V.length td - 1]
    , bench "access/vector" $ nf (map (td V.!)) [0..V.length td - 1]
    , bench "lookupGE/elias-fano" $ nf (map (lookupGE ef)) (V.toList td)
    , bench "lookupGE/vector" $ nf (map binarySearch) (V.toList td)
    , bench "intersection/elias-fano" $ nf intersections $ BV.fromList [unsafeFromVector td1, unsafeFromVector td2]
    , bench "intersection/IntSet" $ nf (IS.toList . uncurry IS.intersection) (IS.fromList $ map fromIntegral $ V.toList td1, IS.fromList $ map fromIntegral $ V.toList td2)
    ]

binarySearch :: Word64 -> Int
binarySearch x = go 0 (V.length td - 1) where
  go l r
    | l >= r = l
    | i <- div (l + r) 2 = case compare x (td V.! i) of
      LT -> go l i
      EQ -> i
      GT -> go (i + 1) r
