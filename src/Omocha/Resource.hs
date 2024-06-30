{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Omocha.Resource where

import Control.Monad.Exception qualified as E (MonadAsyncException (..), MonadException (throw), catch, fromException, throw)
import Control.Monad.Trans.Resource (MonadResource (..), ResourceT, resourceMask, runResourceT)
import Debug.Trace (traceStack)
import GHC.Stack (callStack)
import RIO hiding (traceStack)

newtype GameResource m a = GameResource (ResourceT m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadResource
    )
    via (ResourceT m)

data FooException where
  FooException :: (HasCallStack) => FooException

instance Show FooException where
  show FooException = "FooException\n" <> show callStack

instance E.MonadException (GameResource IO) where
  throw = liftIO . throwIO
  catch (GameResource action) handler = liftIO $ E.catch (runResourceT' action) handler'
    where
      handler' e = case E.fromException e of
        Just e' -> traceStack ("handle Just " ++ show e') $ runResourceT' . unResource $ handler e'
        Nothing -> traceStack ("handle Nothing" ++ show e) liftIO $ E.throw e

instance E.MonadAsyncException (GameResource IO) where
  mask :: ((forall a. GameResource IO a -> GameResource IO a) -> GameResource IO b) -> GameResource IO b
  mask a = GameResource $ resourceMask $ \restore -> unResource (a $ wrapRestore restore)
    where
      wrapRestore restore f = GameResource $ restore (unResource f)

unResource :: GameResource IO a -> ResourceT IO a
unResource (GameResource r) = r

runResourceT' :: (HasCallStack, MonadUnliftIO m) => ResourceT m a -> m a
runResourceT' = traceStack "runResourceT." runResourceT
