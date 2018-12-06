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
  return EliasFano
    { efWidth = width
    , efLength = len
    , efContent = V.fromList $ build $ BitStream (Left 0) (upd counter)
    }
  where
    len = V.length vec
    width = max 1 $ ceiling $ logBase 2 (fromIntegral maxValue / fromIntegral len :: Double)
    upd _ (Left i)
      | i == len = Skip (Right 0)
      | otherwise = Yield width (fromIntegral $ V.unsafeIndex vec i) (Left $! i + 1)
    upd counter (Right i)
      | i == V.length counter = Done
      | otherwise = let n = V.unsafeIndex counter i in Yield (n + 1) (mask n) (Right $! i + 1)
{-# INLINE unsafeFromVectorMax #-}

unsafeFromVector :: V.Vector v Int => v Int -> EliasFano
unsafeFromVector vec
  | V.null vec = unsafeFromVectorMax 1 vec
  | otherwise = unsafeFromVectorMax (V.last vec + 1) vec
{-# SPECIALISE unsafeFromVector :: UV.Vector Int -> EliasFano #-}

data EliasFano = EliasFano
    { efWidth :: !Int
    , efLength :: !Int
    , efContent :: !(UV.Vector Word64)
    }
    deriving Show

(!) :: EliasFano -> Int -> Int
EliasFano width len vec ! i = unsafeShiftL (selectFrom (len * width) vec i - i) width
  .|. fromIntegral (readBits vec width (i * width))

prop_access :: [QC.NonNegative Int] -> QC.NonNegative Int -> QC.Property
prop_access xs i_ = QC.counterexample (show (base, ef, i))
  $ ef ! i == base !! i
  where
    i = QC.getNonNegative i_ `mod` length base
    base = scanl (+) 0 $ map QC.getNonNegative xs
    ef = unsafeFromVector $ UV.fromList base
