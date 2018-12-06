{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Codec.EliasFano (EliasFano(..), unsafeFromVector, (!), prop_access) where

import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Word

import Codec.EliasFano.Internal

import qualified Test.QuickCheck as QC

unsafeFromStreamMax :: Int -> S.Stream m Int -> m EliasFano
unsafeFromStreamMax maxValue (S.Stream upd s0) = runST $ do
  let counterSize = 1 `unsafeShiftL` ceiling (logBase 2 $ fromIntegral len :: Double)
  mcounter <- MV.replicate counterSize 0

  let go s = upd s >>= \case
        S.Done -> pure S.Done
        S.Skip s' -> pure $ S.Skip s'
        S.Yield v s' -> do
          MV.unsafeModify mcounter (+1) (v `unsafeShiftR` efWidth)
          return $ S.Yield (B efWidth (fromIntegral v)) s'
  counter <- UV.unsafeFreeze mcounter
  let efUpper = V.fromList $ build $ BitStream 0 (upper counter)
  return EliasFano
    { efRanks = UV.prescanl (+) 0 $ UV.map popCount upperVec
    , efLower = V.fromList $ build $ BitStream 0 lower
    , ..
    }
  where
    len = V.length vec
    efWidth = max 1 $ ceiling $ logBase 2 (fromIntegral maxValue / fromIntegral len :: Double)
    upper counter i
      | i == V.length counter = S.Done
      | otherwise = let n = V.unsafeIndex counter i in S.Yield (n + 1) (mask n) (i + 1)
{-
unsafeFromStream :: V.Vector v Int => v Int -> EliasFano
unsafeFromStream vec
  | V.null vec = unsafeFromVectorMax 1 vec
  | otherwise = unsafeFromVectorMax (V.last vec + 1) vec
{-# SPECIALISE unsafeFromVector :: UV.Vector Int -> EliasFano #-}
-}
data EliasFano = EliasFano
    { efLength :: !Int
    , efWidth :: !Int
    , efUpper :: !(UV.Vector Word64)
    , efRanks :: !(UV.Vector Int)
    , efLower :: !(UV.Vector Word64)
    }
    deriving Show

(!) :: EliasFano -> Int -> Int
EliasFano _ upper ranks lower ! i = unsafeShiftL (select ranks upper i - i) width
  .|. fromIntegral (readBits lower width (i * width))

prop_access :: [QC.NonNegative Int] -> QC.NonNegative Int -> QC.Property
prop_access xs i_ = QC.counterexample (show (base, ef, i))
  $ ef ! i == base !! i
  where
    i = QC.getNonNegative i_ `mod` length base
    base = scanl (+) 0 $ map QC.getNonNegative xs
    ef = unsafeFromVector $ UV.fromList base
