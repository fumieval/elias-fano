{-# LANGUAGE TemplateHaskell #-}
import Codec.EliasFano
import qualified Data.Vector.Unboxed as UV
import Data.List (findIndex)
import Test.QuickCheck

newtype Monotonic a = Monotonic [a] deriving Show

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Monotonic a) where
  arbitrary = Monotonic . scanl (+) 0 . map getNonNegative <$> arbitrary

prop_lookupGE :: Monotonic Int -> NonNegative Int -> Property
prop_lookupGE (Monotonic xs) (NonNegative x) = case findIndex (>= x) xs of
    Nothing -> discard
    Just j -> lookupGE ef (fromIntegral x) === j
  where
    ef = unsafeFromVector $ UV.fromList $ map fromIntegral xs

prop_access :: Monotonic Int -> NonNegative Int -> Property
prop_access (Monotonic xs) i_ = fromIntegral (ef ! i) === xs !! i
  where
    i = getNonNegative i_ `mod` length xs
    ef = unsafeFromVector $ UV.fromList $ map fromIntegral xs

return []
main = $quickCheckAll
