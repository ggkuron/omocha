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
    n :: select,
    cameraLock :: b,
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
      hardReset = False,
      save = False,
      cameraLock = False,
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
      hardReset = (False, False),
      save = (False, False),
      cameraLock = (False, False),
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
      hardReset = fst i.hardReset,
      save = fst i.save,
      cameraLock = fst i.cameraLock,
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
      hardReset = (a.hardReset, a.hardReset /= b.hardReset),
      save = (a.save, a.save /= b.save),
      cameraLock = (a.cameraLock, a.cameraLock /= b.cameraLock),
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
  _e <- isPressed GLFW.Key'E
  _g <- isPressed GLFW.Key'G
  _d <- isPressed GLFW.Key'D
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
            rotation =
              if
                | w -> Just True
                | f -> Just False
                | otherwise -> Nothing,
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
            confirmSelection = tab,
            cancelSelection = escape
          }
  _ <- liftIO $ keyInputSink keyInput
  return $ if closeKeyPressed then Nothing else t'
  where
    isPressed k = (Just GLFW.KeyState'Pressed ==) <$> GLFW.getKey win k
