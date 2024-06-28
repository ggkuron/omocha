module ShapeSpec (spec) where

import Data.Vector qualified as V
import Omocha.Shape
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "interval" $ do
    it "divids between start and end by the steps " $ do
      interval 4 0 1 `shouldBe` V.fromList ([0, 0.25, 0.5, 0.75, 1] :: [Float])
    it "also supports descent" $ do
      interval 4 1 0 `shouldBe` V.fromList ([1, 0.75, 0.5, 0.25, 0] :: [Float])
    it "always keeps head elment" $ property $ do
      \x y z -> V.head (interval x y (z :: Float)) `shouldBe` y
    it "always keeps last elment" $ property $ do
      \x y z -> V.last (interval x y (z :: Float)) `shouldBe` z
