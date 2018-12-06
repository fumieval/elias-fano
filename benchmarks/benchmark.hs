{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
import Gauge.Main

import Codec.EliasFano
import qualified Data.Binary as B
import qualified Data.Vector.Storable as V

td :: V.Vector Int
td = V.scanl (+) 0 $ V.fromList $ map fromEnum $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

main = do
  let ef = unsafeEncode td
  defaultMain
    [ bench "encode/elias-fano" $ whnf unsafeEncode td
    , bench "encode/vector" $ whnf (V.fromList . V.toList) td
    , bench "access/elias-fano" $ nf (map (access ef)) [0..V.length td - 1]
    , bench "access/vector" $ nf (map (td V.!)) [0..V.length td - 1]
    ]
