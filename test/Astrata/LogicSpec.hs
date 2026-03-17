module Astrata.LogicSpec (spec) where

import Astrata.Logic
import Astrata.MapFile
import Control.Monad.Free (Free (..))
import Data.Vector qualified as V
import Data.Text (Text, isInfixOf)
import Test.Hspec
import Prelude

-- | 純粋インタプリタ: ログを集めながら入力なしで完了まで実行する
runPure :: BattleField -> GameM () -> (BattleField, [Text])
runPure bf prog = go bf prog []
  where
    go bf' (Pure ()) logs = (bf', reverse logs)
    go bf' m logs = case stepGame bf' m of
      StepDone bf'' -> (bf'', reverse logs)
      StepContinue bf'' next -> go bf'' next logs
      StepLog msg bf'' next -> go bf'' next (msg : logs)
      StepNeedInput _ bf'' _ -> (bf'', reverse ("BLOCKED:NeedInput" : logs))

-- | テスト用 BattleField（own の stack にカードを1枚残す）
testBf :: BattleField
testBf =
  let base = initialBattleField
      ownField = base.own
      -- hands の末尾1枚を stack に移してドローテスト可能にする
      h = V.init ownField.hands
      s = V.cons (V.last ownField.hands) ownField.stack
      ownField' = ownField {hands = h, stack = s}
   in base {own = ownField'}

spec :: Spec
spec = do
  describe "drawOneCard" $ do
    it "デッキから手札に1枚移す" $ do
      let field = testBf.own
          field' = drawOneCard field
      V.length field'.hands `shouldBe` V.length field.hands + 1
      V.length field'.stack `shouldBe` V.length field.stack - 1

    it "空デッキでは何もしない" $ do
      let field = testBf.rival -- rival は stack が空
          field' = drawOneCard field
      V.length field'.hands `shouldBe` V.length field.hands
      V.length field'.stack `shouldBe` V.length field.stack

  describe "stepGame" $ do
    it "Pure () は StepDone を返す" $ do
      case stepGame testBf (pure ()) of
        StepDone _ -> pure ()
        _ -> expectationFailure "Expected StepDone"

    it "logMessage は StepLog を返す" $ do
      case stepGame testBf (logMessage "hello") of
        StepLog msg _ _ -> msg `shouldBe` "hello"
        _ -> expectationFailure "Expected StepLog"

    it "getInput は StepNeedInput を返す" $ do
      let prog = do _ <- getInput (ConfirmAction "test?"); pure ()
      case stepGame testBf prog of
        StepNeedInput (ConfirmAction t) _ _ -> t `shouldBe` "test?"
        _ -> expectationFailure "Expected StepNeedInput"

  describe "modifyField / modifyFieldSide" $ do
    it "modifyFieldSide Own drawOneCard でドローが反映される" $ do
      let prog = modifyFieldSide Own drawOneCard
          (bf', _) = runPure testBf prog
      V.length bf'.own.hands `shouldBe` V.length testBf.own.hands + 1

  describe "startPhase" $ do
    it "ログを出力してドローする" $ do
      let (bf', logs) = runPure testBf (startPhase Own)
      logs `shouldSatisfy` any ("Start Phase" `isInfixOf`)
      V.length bf'.own.hands `shouldBe` V.length testBf.own.hands + 1

  describe "runTurn" $ do
    it "5つのフェーズのログが順に出力される" $ do
      let (_, logs) = runPure testBf (runTurn Own)
      length logs `shouldBe` 5
      logs !! 0 `shouldSatisfy` ("Start Phase" `isInfixOf`)
      logs !! 1 `shouldSatisfy` ("Evol Phase" `isInfixOf`)
      logs !! 2 `shouldSatisfy` ("Action Phase" `isInfixOf`)
      logs !! 3 `shouldSatisfy` ("Attack Phase" `isInfixOf`)
      logs !! 4 `shouldSatisfy` ("End Phase" `isInfixOf`)

    it "startPhase でドローが実行される" $ do
      let (bf', _) = runPure testBf (runTurn Own)
      V.length bf'.own.hands `shouldBe` V.length testBf.own.hands + 1

  describe "getInput による中断と再開" $ do
    it "入力要求で中断し、Response で再開できる" $ do
      let prog = do
            resp <- getInput (ConfirmAction "OK?")
            case resp of
              Confirmed True -> logMessage "confirmed"
              _ -> logMessage "other"
      case stepGame testBf prog of
        StepNeedInput (ConfirmAction _) bf' k -> do
          let resumed = k (Confirmed True)
          let (_, logs) = runPure bf' resumed
          logs `shouldBe` ["confirmed"]
        _ -> expectationFailure "Expected StepNeedInput"

  describe "putField" $ do
    it "BattleField を差し替える" $ do
      let prog = do
            bf <- getField
            putField bf {own = bf.rival}
          (bf', _) = runPure testBf prog
      -- own が rival と同じになる
      show bf'.own `shouldBe` show testBf.rival
