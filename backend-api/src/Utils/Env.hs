{-# LANGUAGE OverloadedStrings #-}

module Utils.Env
  ( EnvError,
    getEnvString,
    getEnvStringOrThrow,
    getEnvStringWithDefault,
    getEnvText,
    getEnvTextOrThrow,
    getEnvTextWithDefault,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import System.Environment (lookupEnv)

data EnvError = EnvNotFound String deriving (Show)

instance Exception EnvError

getEnvText :: (MonadIO m) => String -> m (Either EnvError Text)
getEnvText key = liftIO $ do
  maybeValue <- lookupEnv key
  case maybeValue of
    Nothing -> pure $ Left (EnvNotFound $ "Environment variable not found: " ++ key)
    Just value -> pure $ Right (pack value)

getEnvTextWithDefault :: (MonadIO m) => String -> Text -> m Text
getEnvTextWithDefault key defaultValue = liftIO $ do
  maybeValue <- lookupEnv key
  pure $ pack $ fromMaybe (unpack defaultValue) maybeValue

getEnvTextOrThrow :: (MonadIO m) => String -> m Text
getEnvTextOrThrow key = liftIO $ do
  maybeValue <- lookupEnv key
  case maybeValue of
    Nothing -> throwIO $ EnvNotFound $ "Environment variable not found: " ++ key
    Just value -> pure $ pack value

getEnvString :: (MonadIO m) => String -> m (Either EnvError String)
getEnvString key = liftIO $ do
  maybeValue <- lookupEnv key
  case maybeValue of
    Nothing -> pure $ Left (EnvNotFound $ "Environment variable not found: " ++ key)
    Just value -> pure $ Right value

getEnvStringWithDefault :: (MonadIO m) => String -> String -> m String
getEnvStringWithDefault key defaultValue = liftIO $ do
  maybeValue <- lookupEnv key
  pure $ fromMaybe defaultValue maybeValue

getEnvStringOrThrow :: (MonadIO m) => String -> m String
getEnvStringOrThrow key = liftIO $ do
  maybeValue <- lookupEnv key
  case maybeValue of
    Nothing -> throwIO $ EnvNotFound $ "Environment variable not found: " ++ key
    Just value -> pure value
