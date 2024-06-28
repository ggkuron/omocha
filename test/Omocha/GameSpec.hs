module GameSpec (spec) where

import Data.List qualified as L
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Linear.V2
import Omocha.Game
import Omocha.MapFile
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
  describe "parseMapFile" $
    let (w, h) = both fromIntegral mf.size
     in do
          a <- runIO (parseMapFile (V2 w h) mf)
          it "return values" $
            length a `shouldSatisfy` (> 0)

genMatrix :: Gen [[Int]]
genMatrix = do
  Positive size <- arbitrary
  listOf (vectorOf size arbitrary)