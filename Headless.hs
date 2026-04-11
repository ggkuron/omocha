{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Astrata.Headless
import Astrata.Game (updateCardSelection)
import Astrata.MapFile (FieldSide (..), Selection (..), initialBattleField)
import Astrata.Logic (LogicState (..))

main :: IO ()
main = do
  let cfg = HeadlessConfig
        { initialBf  = initialBattleField
        , initialSel = SideSelect Own
        , updateSel  = updateCardSelection
        }
      -- Enter を押してターンを1回実行
      results = runHeadless cfg [initialInput { enter = True }]
  mapM_ printResult results

printResult :: FrameResult -> IO ()
printResult fr = do
  putStrLn $ "Selection: " ++ show fr.selection
  putStrLn $ "Logic: " ++ describeLogic fr.logicState
  putStrLn ""

describeLogic :: LogicState -> String
describeLogic (LogicIdle _) = "Idle"
describeLogic (LogicWaiting req _ _) = "Waiting: " ++ show req
describeLogic (LogicDone logs _) = "Done: " ++ show logs
