{-# LANGUAGE DerivingVia #-}

module Omocha.Context where

import Control.Monad.Exception qualified as E (MonadAsyncException (..), MonadException ())
import Control.Monad.Trans.Resource (MonadResource (..))
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Omocha.Resource
import RIO

newtype GameCtx os a = GameCtx (ContextT GLFW.Handle os (GameResource IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, E.MonadException, E.MonadAsyncException) via (ContextT GLFW.Handle os (GameResource IO))

instance MonadResource (GameCtx os) where
  liftResourceT = GameCtx . lift . liftResourceT

unCtx :: GameCtx os a -> ContextT GLFW.Handle os (GameResource IO) a
unCtx (GameCtx ctx) = ctx
