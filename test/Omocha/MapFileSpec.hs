{-# LANGUAGE QuasiQuotes #-}

module MapFileSpec (spec) where

import Data.Aeson
import Data.BoundingBox qualified as BB
import Data.List qualified as L
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Vector qualified as V
import Linear.V2 (V2 (..))
import Omocha.MapFile
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ
import Prelude

spec :: Spec
spec = do
  describe "parseMap" $ do
    it "wrap as bounding box with row priority" $ do
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [0, 0, 0, 0, 0],
              [0, 1, 0, 0, 0],
              [0, 2, 2, 1, 1],
              [0, 2, 2, 0, 0],
              [0, 0, 0, 0, 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 1 1) (V2 2 2), 1),
                                (BB.Box (V2 1 2) (V2 3 4), 2),
                                (BB.Box (V2 3 2) (V2 5 3), 1)
                              ]
        Left e -> error e
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [0, 0, 0, 0, 0],
              [0, 1, 0, 0, 0],
              [0, 2, 2, 1, 1],
              [0, 2, 2, 0, 0],
              [3, 0, 0, 0, 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 1 1) (V2 2 2), 1),
                                (BB.Box (V2 1 2) (V2 3 4), 2),
                                (BB.Box (V2 3 2) (V2 5 3), 1),
                                (BB.Box (V2 0 4) (V2 1 5), 3)
                              ]
        Left e -> error e
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [0, 0, 0, 0, 0],
              [2, 0, 0, 0, 0],
              [2, 2, 2, 0, 0],
              [2, 2, 2, 0, 0],
              [0, 0, 0, 0, 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 0 1) (V2 1 2), 2),
                                (BB.Box (V2 0 2) (V2 3 4), 2)
                              ]
        Left e -> error e
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [0, 0, 0, 0, 0],
              [0, 0, 0, 0, 0],
              [0, 2, 2, 0, 0],
              [2, 1, 2, 0, 0],
              [0, 0, 0, 0, 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 1 2) (V2 3 3), 2),
                                (BB.Box (V2 0 3) (V2 1 4), 2),
                                (BB.Box (V2 1 3) (V2 2 4), 1),
                                (BB.Box (V2 2 3) (V2 3 4), 2)
                              ]
        Left e -> error e
    it "results do not contain duplicates" $ property $ do
      forAll genNonEmptyMatrix $
        \x ->
          let map = V.fromList $ fmap V.fromList (x :: [[Int]])
           in case parseMap
                (length $ head x, length (x :: [[Int]]))
                map of
                Right r ->
                  r `shouldSatisfy` \r' ->
                    all
                      ( all (<= 1)
                      )
                      [[length . filter (\(bb, _) -> isInside bb x y) $ V.toList r | x <- [0 .. length (x !! y) - 1]] | y <- [0 .. length x - 1]]
                Left e -> error e
    it "represents input" $ property $ do
      forAll genNonEmptyMatrix $
        \x ->
          let map = V.fromList $ fmap V.fromList (x :: [[Int]])
           in case parseMap
                (length $ head x, length (x :: [[Int]]))
                map of
                Right r ->
                  [[fromMaybe 0 (firstJust (\(bb, n) -> if isInside bb x y then Just n else Nothing) $ V.toList r) | x <- [0 .. length (x !! y) - 1]] | y <- [0 .. length x - 1]]
                    `shouldBe` x
                Left e -> error e
  describe "MapFile" $ do
    it "wil be a json representation like this" $ do
      let exampleJson =
            [r|{
                  "mapData":
                  [
                      {
                          "tag": "Tips",
                          "defs":{
                              "1":{"color":[0.4,0.2,0.4,1],"height":1,"tag":"Block", "yOffset": 0},
                              "2":{"color":[0.5,0.5,0.5,1],"height":2,"tag":"Block", "yOffset": 0},
                              "8":{"color":[0.5,0.5,0.5,1],"height":8,"tag":"Block", "yOffset": 4}
                          },
                          "map":[
                              [0,0,0,0,0],
                              [0,1,0,0,0],
                              [0,2,2,1,1],
                              [0,2,2,0,0],
                              [0,8,0,0,0]
                          ]
                      },
                      {
                          "tag": "Fill",
                          "contents":{
                              "color":[0.2,0.4,0.2,1],
                              "height":0.05,
                              "tag":"Block",
                               "yOffset": 0
                          }
                      }
                  ],
                  "size":[5,5]
                }|]
      (decode exampleJson :: Maybe MapFile) `shouldSatisfy` isJust

genMatrix :: Gen [[Int]]
genMatrix = do
  Positive size <- arbitrary
  listOf (vectorOf size arbitrary)

genNonEmptyMatrix :: Gen [[Int]]
genNonEmptyMatrix = do
  Positive size <- arbitrary
  listOf1 (vectorOf size arbitrary)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f
