module Data.BoundingBox.V2Spec (spec) where

import Data.BoundingBox
import Data.BoundingBox.V2
import Linear.V2
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Prelude

spec :: Spec
spec = do
  describe "aabb" $ do
    it "if intersects same point then True" $ do
      aabb (Box (V2 0 0 :: V2 Int) (V2 1 1)) (Box (V2 0 0) (V2 1 1)) `shouldBe` True
      aabb (Box (V2 0 0 :: V2 Int) (V2 1 1)) (Box (V2 1 1) (V2 1 1)) `shouldBe` True
      aabb (Box (V2 2 2 :: V2 Int) (V2 3 3)) (Box (V2 0 0) (V2 1 1)) `shouldBe` False
      aabb (Box (V2 2 2 :: V2 Int) (V2 3 3)) (Box (V2 2 3) (V2 3 3)) `shouldBe` True
      aabb (Box (V2 0 0 :: V2 Int) (V2 3 3)) (Box (V2 1 0) (V2 1 1)) `shouldBe` True
      aabb (Box (V2 0 0 :: V2 Int) (V2 3 3)) (Box (V2 2 0) (V2 1 1)) `shouldBe` True
      aabb (Box (V2 0 0 :: V2 Int) (V2 3 3)) (Box (V2 3 0) (V2 1 1)) `shouldBe` True
      aabb (Box (V2 0 0 :: V2 Int) (V2 1 1)) (Box (V2 2 2) (V2 3 3)) `shouldBe` False
  describe "mult" $ do
    it "if multify all properties" $ do
      Box (V2 1 1) (V2 1 1) `mult` V2 0 0 `shouldBe` (Box (V2 0 0) (V2 0 0) :: Box V2 Int)
      Box (V2 1 1) (V2 1 1) `mult` V2 5 0 `shouldBe` (Box (V2 5 0) (V2 5 0) :: Box V2 Int)
      Box (V2 1 1) (V2 1 1) `mult` V2 0 5 `shouldBe` (Box (V2 0 5) (V2 0 5) :: Box V2 Int)