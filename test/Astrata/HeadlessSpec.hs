module Astrata.HeadlessSpec (spec) where

import Astrata.Game (updateCardSelection)
import Astrata.Headless
import Astrata.Logic (LogicState (..))
import Astrata.MapFile (FieldSide (..), Selection (..), initialBattleField)
import Test.Hspec
import Prelude

-- | Enter を押した入力（edgeInput で True になるよう、前フレームと異なる値にする）
enterInput :: Input
enterInput = initialInput {enter = True}

-- | hardReset を押した入力
hardResetInput :: Input
hardResetInput = initialInput {hardReset = True}

-- | デフォルトの HeadlessConfig（キーボード風 Selection 更新）
defaultConfig :: HeadlessConfig
defaultConfig =
  HeadlessConfig
    { initialBf = initialBattleField,
      initialSel = SideSelect Own,
      updateSel = updateCardSelection
    }

spec :: Spec
spec = do
  describe "runHeadless" $ do
    it "入力なしでは空リストを返す" $ do
      length (runHeadless defaultConfig []) `shouldBe` 0

    it "入力変化なしでは LogicIdle を維持する" $ do
      let results = runHeadless defaultConfig [initialInput, initialInput]
      length results `shouldBe` 2
      all (\r -> isIdle r.logicState) results `shouldBe` True

    it "Enter でターン開始し LogicDone に遷移する" $ do
      let results = runHeadless defaultConfig [enterInput]
      length results `shouldBe` 1
      isDone (head results).logicState `shouldBe` True

    it "hardReset で初期状態に戻る" $ do
      -- まず Enter でターンを回してから hardReset
      let results = runHeadless defaultConfig [enterInput, hardResetInput]
      length results `shouldBe` 2
      isIdle (results !! 1).logicState `shouldBe` True

    it "カスタム Selection 関数で固定 Selection を使える" $ do
      let cfg =
            defaultConfig
              { updateSel = \_ _ _ -> SideSelect Opponent
              }
          results = runHeadless cfg [initialInput]
      (head results).selection `shouldBe` SideSelect Opponent

isIdle :: LogicState -> Bool
isIdle (LogicIdle _) = True
isIdle _ = False

isDone :: LogicState -> Bool
isDone (LogicDone _ _) = True
isDone _ = False
