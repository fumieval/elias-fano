{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Codec.EliasFano (EliasFano(..), unsafeFromVector, (!), prop_access) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Fusion.Bundle.Size as B
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Word

import Codec.EliasFano.Internal

import qualified Test.QuickCheck as QC

unsafeFromStreamNMax :: PrimMonad m => Int -> Int -> S.Stream m Int -> m EliasFano
unsafeFromStreamNMax efLength maxValue (S.Stream upd s0) = do
  let counterSize = 1 `unsafeShiftL` ceiling (logBase 2 $ fromIntegral efLength :: Double)
  mcounter <- MV.replicate counterSize 0

  let go s = upd s >>= \case
        S.Done -> pure S.Done
        S.Skip s' -> pure $ S.Skip s'
        S.Yield v s' -> do
          MV.unsafeModify mcounter (+1) (v `unsafeShiftR` efWidth)
          return $ S.Yield (efWidth `B` fromIntegral v) s'
  counter <- UV.unsafeFreeze mcounter
  mefLower <- GM.munstream $ B.fromStream (chunk64 $ S.Stream go s0)
    $ B.Exact $ (efWidth * efLength + 63) `div` 64
  mefUpper <- GM.munstream $ B.fromStream (chunk64 $ S.Stream (upper counter) 0)
    $ B.Exact $ (efLength + 3) `div` 4
  efUpper <- UV.unsafeFreeze mefUpper
  efLower <- UV.unsafeFreeze mefLower
  return EliasFano
    { efRanks = UV.prescanl (+) 0 $ UV.map popCount efUpper
    , ..
    }
  where
    efWidth = max 1 $ ceiling $ logBase 2 (fromIntegral maxValue / fromIntegral efLength :: Double)
    upper counter i
      | i == V.length counter = pure S.Done
      | otherwise = let !n = V.unsafeIndex counter i in pure $ S.Yield ((n + 1) `B` mask n) (i + 1)
{-# INLINE unsafeFromStreamNMax #-}

unsafeFromVector :: V.Vector v Int => v Int -> EliasFano
unsafeFromVector vec
  | V.null vec = runST $ unsafeFromStreamNMax 0 1 S.empty
  | otherwise = runST $ unsafeFromStreamNMax (V.length vec) (V.last vec + 1)
    $ B.elements $ B.fromVector vec
{-# SPECIALISE unsafeFromVector :: UV.Vector Int -> EliasFano #-}

data EliasFano = EliasFano
    { efLength :: !Int
    , efWidth :: !Int
    , efUpper :: !(UV.Vector Word64)
    , efRanks :: !(UV.Vector Int)
    , efLower :: !(UV.Vector Word64)
    }
    deriving Show

(!) :: EliasFano -> Int -> Int
(!) (EliasFano _ width upper ranks lower) i = unsafeShiftL (select ranks upper i - i) width
  .|. fromIntegral (readBits lower width (i * width))

prop_access :: [QC.NonNegative Int] -> QC.NonNegative Int -> QC.Property
prop_access xs i_ = QC.counterexample (show (base, ef, i))
  $ ef ! i == base !! i
  where
    i = QC.getNonNegative i_ `mod` length base
    base = scanl (+) 0 $ map QC.getNonNegative xs
    ef = unsafeFromVector $ UV.fromList base
