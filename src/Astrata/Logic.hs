{-# LANGUAGE GADTs #-}

module Astrata.Logic
  ( -- * DSL types
    GameF (..),
    GameM,
    InputRequest (..),
    Response (..),

    -- * Smart constructors
    getField,
    putField,
    getInput,
    logMessage,
    modifyField,
    modifyFieldSide,

    -- * Game state
    GameState (..),

    -- * Interpreter
    StepResult (..),
    stepGame,

    -- * Logic state (FRP integration)
    LogicState (..),
    drainSteps,
    logicBattleField,

    -- * Turn logic
    runTurn,
    startPhase,
    evolPhase,
    actionPhase,
    attackPhase,
    endPhase,

    -- * Field operations
    drawOneCard,
  )
where

import Astrata.MapFile
import Control.Monad.Free (Free (..), liftF)
import Data.Vector qualified as V
import RIO

--------------------------------------------------------------------------------
-- InputRequest / Response
--------------------------------------------------------------------------------

data InputRequest
  = SelectHandCard FieldSide
  | SelectBattleSlot FieldSide
  | SelectTarget FieldSide ActionTarget
  | ConfirmAction Text
  deriving (Show, Eq)

data Response
  = SelectedCard CardInstanceId
  | SelectedSlot BattleFieldSlot
  | Confirmed Bool
  | Cancelled
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- GameF functor (GADT)
--------------------------------------------------------------------------------

data GameF next where
  GetField :: (BattleField -> next) -> GameF next
  PutField :: BattleField -> next -> GameF next
  GetInput :: InputRequest -> (Response -> next) -> GameF next
  LogMessage :: Text -> next -> GameF next

deriving instance Functor GameF

type GameM = Free GameF

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

getField :: GameM BattleField
getField = liftF (GetField id)

putField :: BattleField -> GameM ()
putField bf = liftF (PutField bf ())

getInput :: InputRequest -> GameM Response
getInput req = liftF (GetInput req id)

logMessage :: Text -> GameM ()
logMessage msg = liftF (LogMessage msg ())

modifyField :: (BattleField -> BattleField) -> GameM ()
modifyField f = do
  bf <- getField
  putField (f bf)

modifyFieldSide :: FieldSide -> (Field -> Field) -> GameM ()
modifyFieldSide side f = modifyField $ \bf -> case side of
  Own -> bf {own = f bf.own}
  Opponent -> bf {rival = f bf.rival}

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

data GameState = GameState
  { phase :: Phase,
    turn :: FieldSide
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Field operations
--------------------------------------------------------------------------------

-- | デッキ(stack)の先頭1枚を手札(hands)に移す。デッキが空なら何もしない。
drawOneCard :: Field -> Field
drawOneCard field
  | V.null field.stack = field
  | otherwise =
      let drawn = V.head field.stack
          rest = V.tail field.stack
       in field {hands = V.snoc field.hands drawn, stack = rest}

-- | 手札から指定IDのカードを除去する
discardCard :: CardInstanceId -> Field -> Field
discardCard cid field =
  field {hands = V.filter (\c -> instanceId c /= cid) field.hands}

--------------------------------------------------------------------------------
-- Turn / Phase logic
--------------------------------------------------------------------------------

runTurn :: FieldSide -> GameM ()
runTurn side = do
  startPhase side
  evolPhase side
  actionPhase side
  attackPhase side
  endPhase side

startPhase :: FieldSide -> GameM ()
startPhase side = do
  logMessage $ "Start Phase: " <> tshow side
  modifyFieldSide side drawOneCard

evolPhase :: FieldSide -> GameM ()
evolPhase side =
  logMessage $ "Evol Phase: " <> tshow side

actionPhase :: FieldSide -> GameM ()
actionPhase side =
  logMessage $ "Action Phase: " <> tshow side

attackPhase :: FieldSide -> GameM ()
attackPhase side =
  logMessage $ "Attack Phase: " <> tshow side

endPhase :: FieldSide -> GameM ()
endPhase side =
  logMessage $ "End Phase: " <> tshow side

--------------------------------------------------------------------------------
-- Interpreter: step-based execution
--------------------------------------------------------------------------------

-- | 1ステップの実行結果
data StepResult
  = -- | 内部処理完了、次の継続を返す（BattleField を渡して進める）
    StepContinue BattleField (GameM ())
  | -- | ユーザー入力待ち
    StepNeedInput InputRequest BattleField (Response -> GameM ())
  | -- | 完了
    StepDone BattleField
  | -- | ログメッセージ出力
    StepLog Text BattleField (GameM ())

-- | Free monad を1ステップ進める。BattleField は外部から渡す。
stepGame :: BattleField -> GameM () -> StepResult
stepGame bf (Pure ()) = StepDone bf
stepGame bf (Free (GetField k)) = StepContinue bf (k bf)
stepGame _bf (Free (PutField bf' next)) = StepContinue bf' next
stepGame bf (Free (GetInput req k)) = StepNeedInput req bf k
stepGame bf (Free (LogMessage msg next)) = StepLog msg bf next

--------------------------------------------------------------------------------
-- LogicState: FRP シグナルに載せる状態
--------------------------------------------------------------------------------

data LogicState
  = LogicIdle BattleField
  | LogicWaiting InputRequest BattleField (Response -> GameM ())
  | LogicDone [Text] BattleField

-- | StepContinue / StepLog を消化し、StepNeedInput か StepDone で停止。ログを収集する。
drainSteps :: BattleField -> GameM () -> LogicState
drainSteps bf prog = go [] bf prog
  where
    go logs bf' p = case stepGame bf' p of
      StepContinue bf'' next -> go logs bf'' next
      StepLog msg bf'' next -> go (logs ++ [msg]) bf'' next
      StepNeedInput req bf'' k -> LogicWaiting req bf'' k
      StepDone bf'' -> LogicDone logs bf''

-- | LogicState から BattleField を取り出す
logicBattleField :: LogicState -> BattleField
logicBattleField (LogicIdle bf) = bf
logicBattleField (LogicWaiting _ bf _) = bf
logicBattleField (LogicDone _ bf) = bf
