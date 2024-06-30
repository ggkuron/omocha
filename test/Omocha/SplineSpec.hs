module Omocha.SplineSpec (spec) where

import Data.List (nub, sort)
import Data.Vector qualified as V
import Omocha.Spline
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "spline" $ do
    it "include the value used to generate" $ property $ do
      forAll gen2orMoreUniqV2Vector $ \vs ->
        let vs' :: V.Vector Float = V.map fromIntegral vs
            rs = spline vs'
            values = V.map (calcSplines rs) (V.enumFromN 0 (length vs'))
         in values `shouldSatisfy` \rs -> V.any (\r -> V.any (== r) vs') rs

gen2orMoreUniqV2Vector :: Gen (V.Vector Int)
gen2orMoreUniqV2Vector = do
  n <- arbitrary `suchThat` (> 1)
  (V.fromList . nub . sort <$> vectorOf n arbitrary) `suchThat` (\x -> length x > 1)
