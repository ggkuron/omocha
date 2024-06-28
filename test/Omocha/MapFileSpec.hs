{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module MapFileSpec (spec) where

import Data.Aeson
import Data.BoundingBox qualified as BB
import Data.List (sortBy)
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Linear.V2
import Omocha.MapFile
import Omocha.Spline (SplinePairs, isLinear)
import RIO
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ
import Prelude

instance Arbitrary DefId where
  arbitrary = fmap DefId arbitrary

spec :: Spec
spec = do
  describe "parseMap" $ do
    it "wrap as bounding box with row priority" $ do
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0],
              [DefId 0, DefId 1, DefId 0, DefId 0, DefId 0],
              [DefId 0, DefId 2, DefId 2, DefId 1, DefId 1],
              [DefId 0, DefId 2, DefId 2, DefId 0, DefId 0],
              [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 1 1) (V2 2 2), DefId 1),
                                (BB.Box (V2 1 2) (V2 3 4), DefId 2),
                                (BB.Box (V2 3 2) (V2 5 3), DefId 1)
                              ]
        Left e -> error e
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0],
              [DefId 0, DefId 1, DefId 0, DefId 0, DefId 0],
              [DefId 0, DefId 2, DefId 2, DefId 1, DefId 1],
              [DefId 0, DefId 2, DefId 2, DefId 0, DefId 0],
              [DefId 3, DefId 0, DefId 0, DefId 0, DefId 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 1 1) (V2 2 2), DefId 1),
                                (BB.Box (V2 1 2) (V2 3 4), DefId 2),
                                (BB.Box (V2 3 2) (V2 5 3), DefId 1),
                                (BB.Box (V2 0 4) (V2 1 5), DefId 3)
                              ]
        Left e -> error e
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0],
              [DefId 2, DefId 0, DefId 0, DefId 0, DefId 0],
              [DefId 2, DefId 2, DefId 2, DefId 0, DefId 0],
              [DefId 2, DefId 2, DefId 2, DefId 0, DefId 0],
              [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 0 1) (V2 1 2), DefId 2),
                                (BB.Box (V2 0 2) (V2 3 4), DefId 2)
                              ]
        Left e -> error e
      case parseMap
        (5, 5)
        ( V.fromList . fmap V.fromList $
            [ [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0],
              [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0],
              [DefId 0, DefId 2, DefId 2, DefId 0, DefId 0],
              [DefId 2, DefId 1, DefId 2, DefId 0, DefId 0],
              [DefId 0, DefId 0, DefId 0, DefId 0, DefId 0]
            ]
        ) of
        Right x ->
          V.toList x
            `shouldMatchList` [ (BB.Box (V2 1 2) (V2 3 3), DefId 2),
                                (BB.Box (V2 0 3) (V2 1 4), DefId 2),
                                (BB.Box (V2 1 3) (V2 2 4), DefId 1),
                                (BB.Box (V2 2 3) (V2 3 4), DefId 2)
                              ]
        Left e -> error e
    it "results do not contain duplicates" $ property $ do
      forAll genMatrix $
        \x ->
          let map = V.fromList $ fmap V.fromList (x :: [[DefId]])
           in case parseMap (length $ head x, length (x :: [[DefId]])) map of
                Right r ->
                  all (<= 1) ([length . V.filter (\(bb, _) -> isInside bb x y) $ r | y <- [0 .. length x - 1], x <- [0 .. length (x !! y) - 1]])
                    `shouldBe` True
                Left e -> error e
    it "represents input" $ property $ do
      forAll genNonEmptyMatrix $
        \x ->
          let m = V.fromList $ fmap V.fromList (x :: [[DefId]])
           in case parseMap
                (length $ head x, length (x :: [[DefId]]))
                m of
                Right r ->
                  [[fromMaybe (DefId 0) (firstJust (\(bb, n) -> if isInside bb x y then Just n else Nothing) $ V.toList r) | x <- [0 .. length (x !! y) - 1]] | y <- [0 .. length x - 1]]
                    `shouldBe` x
                Left e -> error e
  describe "adjust" $ do
    it "keeps start value" $ property $ do
      forAll
        genAdjustExpected
        ( \(x, ls) ->
            let rs = adjust x (V.fromList ls)
                s = sortBy (compare `on` fst) (filter ((>= 0) . fst) ls)
             in snd (V.head rs) `shouldBe` snd (head s)
        )
    it "keeps last value" $ property $ do
      forAll
        genAdjustExpected
        ( \(x, ls) ->
            let rs = adjust x (V.fromList ls)
                s = sortBy (compare `on` fst) ls
             in snd (V.last rs) `shouldBe` snd (last s)
        )
    it "starts with (0, x)" $ property $ do
      forAll
        genAdjustExpected
        ( \(x, ls) ->
            let rs = adjust x (V.fromList ls)
             in fst (V.head rs) `shouldBe` 0
        )
    it "end with (sx, x)" $ property $ do
      forAll
        genAdjustExpected
        ( \(x, ls) ->
            let rs = adjust x (V.fromList ls)
             in fst (V.last rs) `shouldBe` x
        )
  describe "spline1" $ do
    it "is linear when not axised" $
      property $
        forAll
          ( do
              Positive x <- arbitrary
              Positive y <- arbitrary
              pure (x, y)
          )
          ( \(x :: Int, y :: Int) ->
              spline1 (x, y) Nothing Nothing
                `shouldSatisfy` \(r :: SplinePairs Float) -> uncurry (&&) (both (V.all (uncurry (&&) . both isLinear)) r)
          )

  -- it "has x length" $ property $ do
  --   forAll
  --     genAdjustExpected
  --     ( \(x, ls) ->
  --         adjust x (V.fromList ls)
  --           `shouldSatisfy` (\r -> length r == x + 1)
  --     )

  describe "MapFile" $ do
    it "will be a json representation like this" $ do
      let exampleJson =
            [r|{
                    "mapData":
                    [
                        {
                            "tag": "Tips",
                            "defs":{
                                "1":{"color":[0.4,0.2,0.4,1],"height":1,"tag":"Cube", "yOffset": 0},
                                "2":{"color":[0.5,0.5,0.5,1],"height":2,"tag":"Cube", "yOffset": 0},
                                "8":{"color":[0.5,0.5,0.5,1],"height":8,"tag":"Cube", "yOffset": 4},
                                "3":{"color":[0.5,0.5,0.5,1],"high":1, "low": 0.02, "tag":"Slope", "highEdge": ["Y", true], "yOffset": 0},
                                "4":{
                                  "tag":"Reference",
                                  "contents": [
                                    {
                                      "tag": "Embed",
                                      "contents": {
                                        "size": [2, 2],
                                        "mapData": []
                                      }
                                    },
                                    0
                                  ]
                                }
                            },
                            "maps":[
                              [
                                [0,0,0,0,0],
                                [0,1,0,0,0],
                                [0,2,2,1,1],
                                [0,2,2,4,0],
                                [0,8,0,0,0]
                              ]
                            ]
                        },
                        {
                            "tag": "Fill",
                            "contents": {
                                "color":[0.2,0.4,0.2,1],
                                "height":0.05,
                                "tag":"Cube",
                                "yOffset": 0
                            }
                        }
                    ],
                    "size":[5,5]
                }|]
      let a :: Either String MapFile = eitherDecodeStrict exampleJson
      a `shouldSatisfy` isRight

genMatrix :: forall a. (Arbitrary a) => Gen [[a]]
genMatrix = do
  NonNegative size <- arbitrary
  listOf (vectorOf size arbitrary)

genNonEmptyMatrix :: forall a. (Arbitrary a) => Gen [[a]]
genNonEmptyMatrix = do
  Positive size <- arbitrary
  listOf1 (vectorOf size arbitrary)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

genAdjustExpected :: Gen (Int, [(Int, Float)])
genAdjustExpected = do
  Positive x <- arbitrary
  f <- abs `fmap` (arbitrary :: Gen Int) `suchThat` (\a -> a <= x && a > 0)
  s <- arbitrary
  a <- listOf1 (pure (f, s))
  pure (x, a)