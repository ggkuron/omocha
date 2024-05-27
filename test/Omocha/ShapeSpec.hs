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
    it "head elment should be preserved" $ property $ do
      \x y z -> V.head (interval x y (z :: Float)) `shouldBe` y
    it "last elment should be equal last element" $ property $ do
      \x y z -> V.last (interval x y (z :: Float)) `shouldBe` z
