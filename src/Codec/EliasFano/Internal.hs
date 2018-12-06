{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
module Codec.EliasFano.Internal (Step(..)
  , BitStream(..)
  , mask
  , build
  , readBits
  , selectFrom
  ) where

import Control.Exception (assert)
import Data.Bits
import Data.String
import Data.Word (Word64)
import Data.List.Split (chunksOf)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV

data Step s = Done
  | Skip s
  | Yield !Int !Word64 s

data BitStream = forall s. BitStream s (s -> Step s)

instance Show BitStream where
  show (BitStream s0 f) = show (unwords $ chunksOf 8 $ go s0) where
    go s = case f s of
      Done -> ""
      Skip s' -> go s'
      Yield width w s' -> [if testBit w i then '1' else '0' | i <- [0..width - 1]] ++ go s'

instance IsString BitStream where
  fromString str = BitStream str go where
    go [] = Done
    go ('0' : xs) = Yield 1 0 xs
    go (_ : xs) = Yield 1 1 xs

mask :: Int -> Word64
mask n = unsafeShiftL 1 n - 1
{-# INLINE mask #-}

build :: BitStream -> [Word64]
build (BitStream s0 upd) = go s0 zeroBits 0 where
  go s !acc !len = case upd s of
    Done -> [acc | len > 0]
    Skip s' -> go s' acc len
    Yield width w s' -> case mask width .&. w of
      w' | width + len >= 64 -> (acc .|. unsafeShiftL w' len)
            : go s' (unsafeShiftR w' (64 - len)) (len + width - 64)
         | otherwise -> go s' (acc .|. unsafeShiftL w' len) (len + width)
{-# INLINE build #-}

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

selectFrom :: V.Vector v Word64 => Int -> v Word64 -> Int -> Int
selectFrom offset vec = uncurry (flip $ go 0) $ divMod offset 64 where
  go !acc !ofs !i !q
    | ofs == 0, pv < q = go (acc + 64) ofs (i + 1) (q - pv)
    | ofs == 0 = selectWord64 v q
    | i >= V.length vec - 1 = acc + selectWord64 (v `unsafeShiftR` ofs) q
    | pv' <= q = go (acc + 64) ofs (i + 1) (q - pv')
    | otherwise = acc + selectWord64 v' q
    where
      v = V.unsafeIndex vec i
      pv = popCount v
      v' = v `unsafeShiftR` ofs .|. V.unsafeIndex vec (i + 1) `unsafeShiftL` (64 - ofs)
      pv' = popCount v'
{-# SPECIALISE selectFrom :: Int -> UV.Vector Word64 -> Int -> Int #-}

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
