module Astrata.MapFileSpec (spec) where

import Astrata.MapFile
import Test.Hspec
import Prelude

-- initialBattleField の状態:
--   own  : origin あり、frontL/frontR/backL/backR すべて EmptySlot
--   rival: origin あり、frontL/frontR/backL あり（Assigned）、backR は EmptySlot

testBf :: BattleField
testBf = initialBattleField

spec :: Spec
spec = do
  -- ----------------------------------------------------------------
  -- nextSelection (右キー = 一段深く)
  -- ----------------------------------------------------------------
  describe "nextSelection (右 = 一段深く)" $ do
    describe "SideSelect → FiledSelect" $ do
      it "SideSelect Own → FiledSelect Own BattleZone" $
        nextSelection testBf (SideSelect Own)
          `shouldBe` FiledSelect Own BattleZone
      it "SideSelect Opponent → FiledSelect Opponent BattleZone" $
        nextSelection testBf (SideSelect Opponent)
          `shouldBe` FiledSelect Opponent BattleZone

    describe "FiledSelect → BattleSlotSelect" $ do
      it "FiledSelect Own BattleZone → BattleSlotSelect Own BattleZone OriginSlot" $
        nextSelection testBf (FiledSelect Own BattleZone)
          `shouldBe` BattleSlotSelect Own BattleZone OriginSlot
      it "FiledSelect Opponent AuraZone → BattleSlotSelect Opponent AuraZone OriginSlot" $
        nextSelection testBf (FiledSelect Opponent AuraZone)
          `shouldBe` BattleSlotSelect Opponent AuraZone OriginSlot

    describe "BattleSlotSelect → 変化なし（最深レベル）" $ do
      it "BattleSlotSelect Own BattleZone OriginSlot のまま" $
        nextSelection testBf (BattleSlotSelect Own BattleZone OriginSlot)
          `shouldBe` BattleSlotSelect Own BattleZone OriginSlot

  -- ----------------------------------------------------------------
  -- prevSelection (左キー = 一段上へ)
  -- ----------------------------------------------------------------
  describe "prevSelection (左 = 一段上)" $ do
    describe "BattleSlotSelect → FiledSelect" $ do
      it "BattleSlotSelect Own BattleZone OriginSlot → FiledSelect Own BattleZone" $
        prevSelection testBf (BattleSlotSelect Own BattleZone OriginSlot)
          `shouldBe` FiledSelect Own BattleZone
      it "BattleSlotSelect Opponent AuraZone FrontL → FiledSelect Opponent AuraZone" $
        prevSelection testBf (BattleSlotSelect Opponent AuraZone FrontL)
          `shouldBe` FiledSelect Opponent AuraZone

    describe "FiledSelect → SideSelect" $ do
      it "FiledSelect Own BattleZone → SideSelect Own" $
        prevSelection testBf (FiledSelect Own BattleZone)
          `shouldBe` SideSelect Own
      it "FiledSelect Opponent GuardZone → SideSelect Opponent" $
        prevSelection testBf (FiledSelect Opponent GuardZone)
          `shouldBe` SideSelect Opponent

    describe "SideSelect → 変化なし（最上レベル）" $ do
      it "SideSelect Own のまま" $
        prevSelection testBf (SideSelect Own)
          `shouldBe` SideSelect Own
      it "SideSelect Opponent のまま" $
        prevSelection testBf (SideSelect Opponent)
          `shouldBe` SideSelect Opponent

  -- ----------------------------------------------------------------
  -- nextInLevel (下キー = 同一レベル内で次へ)
  -- ----------------------------------------------------------------
  describe "nextInLevel (下 = 同一レベル内で次)" $ do
    describe "SideSelect" $ do
      it "Own → Opponent" $
        nextInLevel testBf (SideSelect Own) `shouldBe` SideSelect Opponent
      it "Opponent → Own (wrap around)" $
        nextInLevel testBf (SideSelect Opponent) `shouldBe` SideSelect Own

    describe "FiledSelect" $ do
      it "BattleZone → AuraZone" $
        nextInLevel testBf (FiledSelect Own BattleZone) `shouldBe` FiledSelect Own AuraZone
      it "GuardZone → BattleZone (wrap around)" $
        nextInLevel testBf (FiledSelect Own GuardZone) `shouldBe` FiledSelect Own BattleZone

    describe "BattleSlotSelect (rival: frontL/frontR/backL Assigned, backR EmptySlot)" $ do
      it "OriginSlot → FrontL" $
        nextInLevel testBf (BattleSlotSelect Opponent BattleZone OriginSlot)
          `shouldBe` BattleSlotSelect Opponent BattleZone FrontL
      it "FrontL → FrontR" $
        nextInLevel testBf (BattleSlotSelect Opponent BattleZone FrontL)
          `shouldBe` BattleSlotSelect Opponent BattleZone FrontR
      it "FrontR → BackL" $
        nextInLevel testBf (BattleSlotSelect Opponent BattleZone FrontR)
          `shouldBe` BattleSlotSelect Opponent BattleZone BackL
      it "BackL → OriginSlot (BackR は EmptySlot なのでスキップ)" $
        nextInLevel testBf (BattleSlotSelect Opponent BattleZone BackL)
          `shouldBe` BattleSlotSelect Opponent BattleZone OriginSlot

    describe "BattleSlotSelect (own: 全スロット EmptySlot)" $ do
      it "OriginSlot → OriginSlot（他に Assigned なし）" $
        nextInLevel testBf (BattleSlotSelect Own BattleZone OriginSlot)
          `shouldBe` BattleSlotSelect Own BattleZone OriginSlot

  -- ----------------------------------------------------------------
  -- prevInLevel (上キー = 同一レベル内で前へ)
  -- ----------------------------------------------------------------
  describe "prevInLevel (上 = 同一レベル内で前)" $ do
    describe "SideSelect" $ do
      it "Opponent → Own" $
        prevInLevel testBf (SideSelect Opponent) `shouldBe` SideSelect Own
      it "Own → Opponent (wrap around)" $
        prevInLevel testBf (SideSelect Own) `shouldBe` SideSelect Opponent

    describe "FiledSelect" $ do
      it "AuraZone → BattleZone" $
        prevInLevel testBf (FiledSelect Own AuraZone) `shouldBe` FiledSelect Own BattleZone
      it "BattleZone → GuardZone (wrap around)" $
        prevInLevel testBf (FiledSelect Own BattleZone) `shouldBe` FiledSelect Own GuardZone

    describe "BattleSlotSelect (rival: frontL/frontR/backL Assigned, backR EmptySlot)" $ do
      it "OriginSlot → BackL (BackR は EmptySlot なのでスキップ)" $
        prevInLevel testBf (BattleSlotSelect Opponent BattleZone OriginSlot)
          `shouldBe` BattleSlotSelect Opponent BattleZone BackL
      it "FrontL → OriginSlot" $
        prevInLevel testBf (BattleSlotSelect Opponent BattleZone FrontL)
          `shouldBe` BattleSlotSelect Opponent BattleZone OriginSlot
      it "BackL → FrontR" $
        prevInLevel testBf (BattleSlotSelect Opponent BattleZone BackL)
          `shouldBe` BattleSlotSelect Opponent BattleZone FrontR

  -- ----------------------------------------------------------------
  -- 階層の往復対称性
  -- ----------------------------------------------------------------
  describe "next/prev 階層の往復" $ do
    it "SideSelect: next して prev で元に戻る" $
      prevSelection testBf (nextSelection testBf (SideSelect Own))
        `shouldBe` SideSelect Own
    it "FiledSelect: next して prev で元に戻る" $
      prevSelection testBf (nextSelection testBf (FiledSelect Own BattleZone))
        `shouldBe` FiledSelect Own BattleZone
    it "BattleSlotSelect: prev して next で元に戻る" $
      nextSelection testBf (prevSelection testBf (BattleSlotSelect Opponent BattleZone FrontL))
        `shouldBe` BattleSlotSelect Opponent BattleZone OriginSlot
      -- BattleSlotSelect → prev → FiledSelect → next → BattleSlotSelect OriginSlot（FrontL には戻らない）

  -- ----------------------------------------------------------------
  -- nextInLevel/prevInLevel 同一レベル対称性
  -- ----------------------------------------------------------------
  describe "nextInLevel/prevInLevel 対称性" $ do
    it "SideSelect: nextInLevel して prevInLevel で元に戻る" $
      prevInLevel testBf (nextInLevel testBf (SideSelect Own)) `shouldBe` SideSelect Own
    it "FiledSelect: nextInLevel して prevInLevel で元に戻る" $
      prevInLevel testBf (nextInLevel testBf (FiledSelect Own BattleZone))
        `shouldBe` FiledSelect Own BattleZone
    it "BattleSlotSelect/rival: nextInLevel して prevInLevel で元に戻る (OriginSlot)" $
      prevInLevel testBf (nextInLevel testBf (BattleSlotSelect Opponent BattleZone OriginSlot))
        `shouldBe` BattleSlotSelect Opponent BattleZone OriginSlot
