module Astrata.Headless
  ( HeadlessConfig (..),
    FrameResult (..),
    runHeadless,
    -- * Re-exports from UserInput
    Input,
    InputG (..),
    initialInput,
  )
where

import Astrata.Game (advanceLogic)
import Astrata.Logic (LogicState (..), logicBattleField)
import Astrata.MapFile (BattleField, Selection)
import Astrata.UserInput (Input, InputE, InputG (..), diffInput, initialInput, initialInputE, onlyInput)

-- | ヘッドレス実行の設定
data HeadlessConfig = HeadlessConfig
  { initialBf :: BattleField,
    initialSel :: Selection,
    updateSel :: Selection -> InputE -> BattleField -> Selection
  }

-- | 各フレームの結果
data FrameResult = FrameResult
  { logicState :: LogicState,
    selection :: Selection,
    battleField :: BattleField
  }

-- | ヘッドレスでロジックを実行する。入力列を受け取り、各フレームの結果を返す。
runHeadless :: HeadlessConfig -> [Input] -> [FrameResult]
runHeadless cfg inputs = go initialInputE (LogicIdle cfg.initialBf) cfg.initialSel inputs
  where
    go _ _ _ [] = []
    go prevInputE logicSt sel (inp : rest) =
      let inputE = diffInput inp (onlyInput prevInputE)
          bf = logicBattleField logicSt
          sel' = cfg.updateSel sel inputE bf
          logicSt' = advanceLogic inputE sel' logicSt
          result =
            FrameResult
              { logicState = logicSt',
                selection = sel',
                battleField = logicBattleField logicSt'
              }
       in result : go inputE logicSt' sel' rest
