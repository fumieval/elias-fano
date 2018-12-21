{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Codec.EliasFano (EliasFano(..)
    , unsafeFromVector
    , (!)
    , lookupGE) where

import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Fusion.Bundle.Size as B
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Word

import Codec.EliasFano.Internal

data EncoderState s = ESCont !Int !Word64 !Int | ESDone

unsafeFromVector :: V.Vector v Word64 => v Word64 -> EliasFano
unsafeFromVector vec = runST $ do
  efLower <- fromStream' ((efWidth * efLength + 63) `div` 64)
    $ chunk64 $ S.map (B efWidth . fromIntegral) $ B.elements $ B.fromVector vec
  efUpper <- fromStream' ((efLength + 3) `div` 4)
    $ chunk64 $ unary $ S.Stream upd $ ESCont 0 0 0
  return EliasFano
    { efRanks = UV.prescanl (+) 0 $ UV.map popCount efUpper
    , ..
    }
  where
    upd ESDone = pure S.Done
    upd (ESCont i current n)
      | current > maxValue `unsafeShiftR` efWidth = pure $ S.Yield n ESDone
      | otherwise = pure $ case fromIntegral $ V.unsafeIndex vec i `unsafeShiftR` efWidth of
        u | u == current -> S.Skip $ ESCont (i + 1) current (n + 1)
          | otherwise -> S.Yield n (ESCont i (current + 1) 0)

    efLength = V.length vec

    fromStream' len s = GM.munstream (B.fromStream s (B.Exact len))
      >>= UV.unsafeFreeze
    {-# INLINE fromStream' #-}

    maxValue
      | V.null vec = 1
      | otherwise = V.last vec + 1
    efWidth = max 1 $ ceiling $ logBase 2 (fromIntegral maxValue / fromIntegral efLength :: Double)
{-# SPECIALISE unsafeFromVector :: UV.Vector Word64 -> EliasFano #-}

data EliasFano = EliasFano
    { efLength :: !Int
    , efWidth :: !Int
    , efUpper :: !(UV.Vector Word64)
    , efRanks :: !(UV.Vector Int)
    , efLower :: !(UV.Vector Word64)
    }
    deriving Show

(!) :: EliasFano -> Int -> Word64
(!) (EliasFano _ width upper ranks lower) i = fromIntegral (unsafeShiftL (select ranks upper i - i) width)
  .|. readBits lower width (i * width)

lookupGE :: EliasFano -> Word64 -> Int
lookupGE ef@EliasFano{..} x
  | x <= ef ! l0 = l0
  | otherwise = go l0 r0
  where
    go l r | l >= r = l
    go l r = case div (l + r) 2 of
      i -> case compare low (readBits efLower efWidth (i * efWidth)) of
        LT -> go l i
        EQ -> i
        GT -> go (i + 1) r

    high = fromIntegral $ x `unsafeShiftR` efWidth
    low = x .&. mask efWidth

    l0
      | high == 0 = 0
      | otherwise = select0 efRanks efUpper (high - 1) - high + 1
    r0 = select0 efRanks efUpper high - high
