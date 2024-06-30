module Omocha.UserInput where

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW qualified as GLFW
import RIO

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)

data Input = Input
  { direction1 :: Maybe Direction,
    direction2 :: Maybe Direction,
    reset :: Bool,
    save :: Bool,
    stop :: Bool,
    hardReset :: Bool,
    enter :: Bool,
    rotation :: Maybe Bool,
    speedUp :: Bool,
    hover :: Bool,
    n :: Maybe Int,
    jump :: Bool
  }

initialInput :: Input
initialInput =
  Input
    { direction1 = Nothing,
      direction2 = Nothing,
      stop = False,
      reset = False,
      enter = False,
      rotation = Nothing,
      n = Nothing,
      hover = False,
      hardReset = False,
      save = False,
      speedUp = False,
      jump = False
    }

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
  x <- isPressed GLFW.Key'X
  w <- isPressed GLFW.Key'W
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

  let keyInput =
        Input
          { direction1 =
              if
                | ctl -> Nothing
                | h -> Just DirLeft
                | j -> Just DirDown
                | k -> Just DirUp
                | l -> Just DirRight
                | otherwise -> Nothing,
            direction2 =
              if
                | ctl -> Nothing
                | s -> Just DirLeft
                | d -> Just DirDown
                | e -> Just DirUp
                | g -> Just DirRight
                | otherwise -> Nothing,
            reset = not ctl && space,
            hardReset = ctl && space,
            enter = f,
            save = ctl && s,
            stop = c,
            speedUp = shift,
            rotation =
              if
                | z -> Just True
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
                | otherwise -> Nothing
          }
  _ <- liftIO $ keyInputSink keyInput
  return $ if closeKeyPressed then Nothing else t'
  where
    isPressed k = (Just GLFW.KeyState'Pressed ==) <$> GLFW.getKey win k
