module Omocha.UserInput where

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW qualified as GLFW
import RIO

data Direction = DirUp | DirDown | DirLeft | DirRight

data Input = Input
  { direction1 :: Maybe Direction,
    direction2 :: Maybe Direction,
    reset :: Bool,
    save :: Bool,
    hardReset :: Bool,
    enter :: Bool,
    rotation :: Maybe Bool,
    n :: Maybe Int
  }

readInput ::
  MonadIO m =>
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
  w <- isPressed GLFW.Key'W
  a <- isPressed GLFW.Key'A
  s <- isPressed GLFW.Key'S
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
                  | a -> Just DirLeft
                  | s -> Just DirDown
                  | w -> Just DirUp
                  | d -> Just DirRight
                  | otherwise -> Nothing,
            reset = not ctl && space,
            hardReset = ctl && space,
            enter = f,
            save = ctl && s,
            rotation =
              if
                  | z -> Just True
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
                  | otherwise -> Nothing
          }
  _ <- liftIO $ keyInputSink keyInput
  return $ if closeKeyPressed then Nothing else t'
  where
    isPressed k = (Just GLFW.KeyState'Pressed ==) <$> GLFW.getKey win k
