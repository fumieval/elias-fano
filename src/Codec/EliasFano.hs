module Codec.EliasFano (EliasFano(..), unsafeEncode, access, prop_access) where

import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Data.Word

import Codec.EliasFano.Internal

import qualified Test.QuickCheck as QC

unsafeEncodeMax :: Int -> V.Vector Int -> EliasFano
unsafeEncodeMax maxValue vec = runST $ do
  let counterSize = 1 `unsafeShiftL` ceiling (logBase 2 $ fromIntegral len :: Double)
  mcounter <- MV.replicate counterSize 0
  V.forM_ vec $ \v -> MV.unsafeModify mcounter (+1) (v `unsafeShiftR` width)
  counter <- V.unsafeFreeze mcounter
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

unsafeEncode :: V.Vector Int -> EliasFano
unsafeEncode vec
  | V.null vec = unsafeEncodeMax 1 vec
  | otherwise = unsafeEncodeMax (V.last vec + 1) vec

data EliasFano = EliasFano
    { efWidth :: !Int
    , efLength :: !Int
    , efContent :: !(V.Vector Word64)
    }
    deriving Show

access :: EliasFano -> Int -> Int
access (EliasFano width len vec) i = unsafeShiftL (selectFrom (len * width) vec i - i) width
  .|. fromIntegral (readBits vec width (i * width))

prop_access :: [QC.NonNegative Int] -> QC.NonNegative Int -> QC.Property
prop_access xs i_ = QC.counterexample (show (base, ef, i))
  $ access ef i == base !! i
  where
    i = QC.getNonNegative i_ `mod` length base
    base = scanl (+) 0 $ map QC.getNonNegative xs
    ef = unsafeEncode $ V.fromList base
