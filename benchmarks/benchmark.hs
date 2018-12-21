{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
import Gauge.Main

import Codec.EliasFano
import qualified Data.Binary as B
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)

td :: V.Vector Word64
td = V.scanl (+) 0 $ V.fromList $ map (toEnum . fromEnum) $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

main = do
  let ef = unsafeFromVector td
  defaultMain
    [ bench "encode/elias-fano" $ whnf unsafeFromVector td
    , bench "encode/vector" $ whnf V.sum td
    , bench "access/elias-fano" $ nf (map (ef!)) [0..V.length td - 1]
    , bench "access/vector" $ nf (map (td V.!)) [0..V.length td - 1]
    ]
