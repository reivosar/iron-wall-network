{-# LANGUAGE FlexibleContexts #-}

module Utils.UUIDGenerator (generateUUID, generateUUIDText) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)

generateUUID :: (MonadIO m) => m UUID
generateUUID = liftIO nextRandom

generateUUIDText :: (MonadIO m) => m Text
generateUUIDText = toText <$> generateUUID
