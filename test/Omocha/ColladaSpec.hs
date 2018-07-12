module ColladaSpec (spec) where

import Test.Hspec
import Omocha.Collada


spec :: Spec
spec = do
    describe "splitIn" $ do
        it "devide array into n stride groups" $ do
            splitIn 3 [0..10] `shouldBe` [[0,3,6,9], [1,4,7,10], [2,5,8]]
            splitIn 2 [0..10] `shouldBe` [[0,2,4,6,8,10], [1,3,5,7,9]]
            splitIn 1 [0..10] `shouldBe` [[0..10]]
    
