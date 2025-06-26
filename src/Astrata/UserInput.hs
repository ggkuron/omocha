module Astrata.UserInput where

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW qualified as GLFW
import RIO

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)

type Input = InputG (Maybe Direction) Bool (Maybe Bool) (Maybe Int)

data InputG d b r select = Input
  { direction :: d,
    reset :: b,
    save :: b,
    stop :: b,
    hardReset :: b,
    enter :: b,
    rotation :: r,
    speedUp :: b,
    hover :: b,
    n :: select,
    jump :: b,
    cameraLock :: b,
    selectCard :: select,
    confirmSelection :: b,
    cancelSelection :: b
  }

type InputE = InputG (Maybe Direction, Bool) (Bool, Bool) (Maybe Bool, Bool) (Maybe Int, Bool)

initialInput :: Input
initialInput =
  Input
    { direction = Nothing,
      stop = False,
      reset = False,
      enter = False,
      rotation = Nothing,
      n = Nothing,
      hover = False,
      hardReset = False,
      save = False,
      speedUp = False,
      jump = False,
      cameraLock = False,
      selectCard = Nothing,
      confirmSelection = False,
      cancelSelection = False
    }

initialInputE :: InputE
initialInputE =
  Input
    { direction = (Nothing, False),
      stop = (False, False),
      reset = (False, False),
      enter = (False, False),
      rotation = (Nothing, False),
      n = (Nothing, False),
      hover = (False, False),
      hardReset = (False, False),
      save = (False, False),
      speedUp = (False, False),
      jump = (False, False),
      cameraLock = (False, False),
      selectCard = (Nothing, False),
      confirmSelection = (False, False),
      cancelSelection = (False, False)
    }

onlyInput :: InputE -> Input
onlyInput i =
  Input
    { direction = fst i.direction,
      stop = fst i.stop,
      reset = fst i.reset,
      enter = fst i.enter,
      rotation = fst i.rotation,
      n = fst i.n,
      hover = fst i.hover,
      hardReset = fst i.hardReset,
      save = fst i.save,
      speedUp = fst i.speedUp,
      jump = fst i.jump,
      cameraLock = fst i.cameraLock,
      selectCard = fst i.selectCard,
      confirmSelection = fst i.confirmSelection,
      cancelSelection = fst i.cancelSelection
    }

diffInput :: Input -> Input -> InputE
diffInput a b =
  Input
    { direction = (a.direction, a.direction /= b.direction),
      stop = (a.stop, a.stop /= b.stop),
      reset = (a.reset, a.reset /= b.reset),
      enter = (a.enter, a.enter /= b.enter),
      rotation = (a.rotation, a.rotation /= b.rotation),
      n = (a.n, a.n /= b.n),
      hover = (a.hover, a.hover /= b.hover),
      hardReset = (a.hardReset, a.hardReset /= b.hardReset),
      save = (a.save, a.save /= b.save),
      speedUp = (a.speedUp, a.speedUp /= b.speedUp),
      jump = (a.jump, a.jump /= b.jump),
      cameraLock = (a.cameraLock, a.cameraLock /= b.cameraLock),
      selectCard = (a.selectCard, a.selectCard /= b.selectCard),
      confirmSelection = (a.confirmSelection, a.confirmSelection /= b.confirmSelection),
      cancelSelection = (a.cancelSelection, a.cancelSelection /= b.cancelSelection)
    }

edgeInput :: (Bool, Bool) -> Bool
edgeInput (a, b) = a && b

justInput :: (a, Bool) -> a
justInput = fst

readInput ::
  (MonadIO m) =>
  Window os c ds ->
  (Input -> IO a) ->
  ContextT GLFW.Handle os m (Maybe Double)
readInput win keyInputSink = do
  t' <- liftIO GLFW.getTime
  closeKeyPressed <- isPressed GLFW.Key'Escape
  h <- isPressed GLFW.Key'H
  j <- isPressed GLFW.Key'J
  k <- isPressed GLFW.Key'K
  l <- isPressed GLFW.Key'L
  space <- isPressed GLFW.Key'Space
  a <- isPressed GLFW.Key'A
  w <- isPressed GLFW.Key'W
  x <- isPressed GLFW.Key'X
  c <- isPressed GLFW.Key'C
  s <- isPressed GLFW.Key'S
  e <- isPressed GLFW.Key'E
  g <- isPressed GLFW.Key'G
  d <- isPressed GLFW.Key'D
  f <- isPressed GLFW.Key'F
  z <- isPressed GLFW.Key'Z
  n1 <- isPressed GLFW.Key'1
  n2 <- isPressed GLFW.Key'2
  n3 <- isPressed GLFW.Key'3
  n4 <- isPressed GLFW.Key'4
  n5 <- isPressed GLFW.Key'5
  n6 <- isPressed GLFW.Key'6
  n7 <- isPressed GLFW.Key'7
  n8 <- isPressed GLFW.Key'8
  n9 <- isPressed GLFW.Key'9
  n0 <- isPressed GLFW.Key'0
  ctl <- isPressed GLFW.Key'LeftControl
  shift <- isPressed GLFW.Key'LeftShift
  tab <- isPressed GLFW.Key'Tab
  escape <- isPressed GLFW.Key'Escape

  let keyInput =
        Input
          { direction =
              if
                | ctl -> Nothing
                | h -> Just DirLeft
                | j -> Just DirDown
                | k -> Just DirUp
                | l -> Just DirRight
                | otherwise -> Nothing,
            cameraLock = z,
            reset = not shift && space,
            hardReset = shift && space,
            enter = f,
            save = ctl && s,
            stop = c,
            speedUp = shift,
            rotation =
              if
                | w -> Just True
                | f -> Just False
                | otherwise -> Nothing,
            jump = a,
            hover = x,
            n =
              if
                | n0 -> Just 0
                | n1 -> Just 1
                | n2 -> Just 2
                | n3 -> Just 3
                | n4 -> Just 4
                | n5 -> Just 5
                | n6 -> Just 6
                | n7 -> Just 7
                | n8 -> Just 8
                | n9 -> Just 9
                | otherwise -> Nothing,
            selectCard =
              if
                | not ctl && n0 -> Just 0
                | not ctl && n1 -> Just 1
                | not ctl && n2 -> Just 2
                | not ctl && n3 -> Just 3
                | not ctl && n4 -> Just 4
                | not ctl && n5 -> Just 5
                | not ctl && n6 -> Just 6
                | not ctl && n7 -> Just 7
                | not ctl && n8 -> Just 8
                | not ctl && n9 -> Just 9
                | otherwise -> Nothing,
            confirmSelection = tab,
            cancelSelection = escape
          }
  _ <- liftIO $ keyInputSink keyInput
  return $ if closeKeyPressed then Nothing else t'
  where
    isPressed k = (Just GLFW.KeyState'Pressed ==) <$> GLFW.getKey win k
