{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Codec.EliasFano.Internal (B(..), chunk64
  , mask
  , readBits
  , select
  ) where

import Control.Exception (assert)
import Data.Bits
import Data.Word (Word64)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Fusion.Stream.Monadic as S

data B = B !Int !Word64

data Chunker s = Chunker s !Word64 !Int
  | ChunkerDone

chunk64 :: Applicative m => S.Stream m B -> S.Stream m Word64
chunk64 (S.Stream upd s0) = S.Stream go $ Chunker s0 zeroBits 0 where
  go ChunkerDone = pure S.Done
  go (Chunker s acc len) = flip fmap (upd s) $ \case
    S.Done -> S.Yield acc ChunkerDone
    S.Skip s' -> S.Skip $ Chunker s' acc len
    S.Yield (B width w) s' -> case mask width .&. w of
      w' | width + len >= 64 -> S.Yield (acc .|. unsafeShiftL w' len)
            $ Chunker s' (unsafeShiftR w' (64 - len)) (len + width - 64)
         | otherwise -> S.Skip $ Chunker s' (acc .|. unsafeShiftL w' len) (len + width)
{-# INLINE chunk64 #-}

mask :: Int -> Word64
mask n = unsafeShiftL 1 n - 1
{-# INLINE mask #-}

readBits :: V.Vector v Word64 => v Word64 -> Int -> Int -> Word64
readBits vec width pos
  | b + width > 64 = unsafeShiftL extra (64 - b) .|. base
  | otherwise = base
  where
    i = unsafeShiftR pos 6
    b = pos .&. 63
    base = (V.unsafeIndex vec i `unsafeShiftR` b) .&. mask width
    extra = V.unsafeIndex vec (i + 1) .&. mask (width + b - 64)
{-# SPECIALISE readBits :: UV.Vector Word64 -> Int -> Int -> Word64 #-}

select :: (V.Vector v Int, V.Vector v Word64) => v Int -> v Word64 -> Int -> Int
select ranks vec q = go 0 (V.length ranks - 1) where
  go l r
    | l >= r = selectWord64 v (q - V.unsafeIndex ranks l) + 64 * l
    | q < V.unsafeIndex ranks (i + 1) = go l i
    | otherwise = go (i + 1) r
    where
      i = div (l + r) 2
      v = V.unsafeIndex vec i
{-# SPECIALISE select :: UV.Vector Int -> UV.Vector Word64 -> Int -> Int #-}

-- | Convert a word of various bits into a word where each byte contains the count of bits in the corresponding original byte
--
-- @'popCount' = 'byteSum' . 'byteCounts'@
byteCounts :: Word64 -> Word64
byteCounts a = d .&. lsns where
  threes  = 0x3333333333333333
  as      = 0xAAAAAAAAAAAAAAAA
  lsbs    = 0x0101010101010101
  lsns = 0x0f * lsbs
  b = a - shiftR (a .&. as) 1
  c = (b .&. threes) + (shiftR b 2 .&. threes)
  d = c + shiftR c 4

-- | signed compare byte by byte, returning whether or not the result is less than or equal to
-- the corresponding byte in the other word as the least significant bit of each byte
leq8 :: Word64 -> Word64 -> Word64
leq8 x y = shiftR (w .&. msbs) 7 where
  msbs = 0x8080808080808080
  z = (y .|. msbs) - (x .&. complement msbs)
  w = x `xor` y `xor` z

-- https://github.com/ekmett/succinct/blob/7e884138c2e943f5ca08f56b58b409d08b870ab9/src/Succinct/Internal/Broadword.hs
nonzero8 :: Word64 -> Word64
nonzero8 x = shiftR ((x .|. ((x .|. msbs) - lsbs)) .&. msbs) 7 where
  msbs = 0x8080808080808080
  lsbs = 0x0101010101010101

-- https://github.com/ekmett/succinct/blob/7e884138c2e943f5ca08f56b58b409d08b870ab9/src/Succinct/Internal/Broadword.hs
selectWord64 :: Word64 -> Int -> Int
selectWord64 x k = assert (k < popCount x) (place + offset) where
  wk      = fromIntegral k
  lsbs    = 0x0101010101010101
  hi      = 0xFFFFFFFFFFFFFFF8
  inc     = 0x8040201008040201
  sums    = byteCounts x * lsbs
  steps   = wk * lsbs
  place   = fromIntegral $ shiftR (leq8 sums steps * lsbs) 53 .&. hi
  br      = wk - (shiftR (shiftL sums 8) place .&. 0xFF)
  spread  = (shiftR x place .&. 0xFF) * lsbs
  bitSums = nonzero8 (spread .&. inc) * lsbs
  offset  = fromIntegral $ shiftR (leq8 bitSums (br * lsbs) * lsbs) 56
