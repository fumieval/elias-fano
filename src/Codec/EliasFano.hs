{-# LANGUAGE FlexibleContexts #-}
module Codec.EliasFano (EliasFano(..), unsafeFromVector, (!), prop_access) where

import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import Codec.EliasFano.Internal

import qualified Test.QuickCheck as QC

unsafeFromVectorMax :: V.Vector v Int => Int -> v Int -> EliasFano
unsafeFromVectorMax maxValue vec = runST $ do
  let counterSize = 1 `unsafeShiftL` ceiling (logBase 2 $ fromIntegral len :: Double)
  mcounter <- MV.replicate counterSize 0
  V.forM_ vec $ \v -> MV.unsafeModify mcounter (+1) (v `unsafeShiftR` width)
  counter <- UV.unsafeFreeze mcounter
  let upperVec = V.fromList $ build $ BitStream 0 (upper counter)
  return EliasFano
    { efWidth = width
    , efUpper = upperVec
    , efRanks = UV.prescanl (+) 0 $ UV.map popCount upperVec
    , efLower = V.fromList $ build $ BitStream 0 lower
    }
  where
    len = V.length vec
    width = max 1 $ ceiling $ logBase 2 (fromIntegral maxValue / fromIntegral len :: Double)
    lower i
      | i == len = Done
      | otherwise = Yield width (fromIntegral $ V.unsafeIndex vec i) (i + 1)
    upper counter i
      | i == V.length counter = Done
      | otherwise = let n = V.unsafeIndex counter i in Yield (n + 1) (mask n) (i + 1)
{-# INLINE unsafeFromVectorMax #-}

unsafeFromVector :: V.Vector v Int => v Int -> EliasFano
unsafeFromVector vec
  | V.null vec = unsafeFromVectorMax 1 vec
  | otherwise = unsafeFromVectorMax (V.last vec + 1) vec
{-# SPECIALISE unsafeFromVector :: UV.Vector Int -> EliasFano #-}

data EliasFano = EliasFano
    { efWidth :: !Int
    , efUpper :: !(UV.Vector Word64)
    , efRanks :: !(UV.Vector Int)
    , efLower :: !(UV.Vector Word64)
    }
    deriving Show

(!) :: EliasFano -> Int -> Int
EliasFano width upper ranks lower ! i = unsafeShiftL (select ranks upper i - i) width
  .|. fromIntegral (readBits lower width (i * width))

prop_access :: [QC.NonNegative Int] -> QC.NonNegative Int -> QC.Property
prop_access xs i_ = QC.counterexample (show (base, ef, i))
  $ ef ! i == base !! i
  where
    i = QC.getNonNegative i_ `mod` length base
    base = scanl (+) 0 $ map QC.getNonNegative xs
    ef = unsafeFromVector $ UV.fromList base
