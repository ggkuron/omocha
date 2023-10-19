module GameSpec (spec) where

import Data.BoundingBox qualified as BB
import Data.List qualified as L
import Data.Vector qualified as V
import Omocha.Game
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "transpose" $ do
    it "behaves like transpose in Data.List" $ property $ do
      forAll genMatrix $
        \x -> V.toList (V.map V.toList $ transpose $ V.fromList $ map V.fromList (x :: [[Int]])) `shouldBe` L.transpose x
    it "and transposed back to original" $ property $ do
      forAll genMatrix $
        \x -> V.toList (V.map V.toList $ transpose (transpose $ V.fromList $ map V.fromList (x :: [[Int]]))) `shouldBe` (x :: [[Int]])

genMatrix :: Gen [[Int]]
genMatrix = do
  Positive size <- arbitrary
  listOf (vectorOf size arbitrary)

genNonEmptyMatrix :: Gen [[Int]]
genNonEmptyMatrix = do
  Positive size <- arbitrary
  listOf1 (vectorOf size arbitrary)
