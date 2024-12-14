module Middleware.OperatorIdService
  ( OperatorIdService (..),
  )
where

import Control.Exception (SomeException)
import Data.Text (Text)

class OperatorIdService m where
  getOperatorIdByUserName :: Text -> m (Either SomeException (Maybe Int))
  getOperatorIdByAccessToken :: Text -> m (Either SomeException (Maybe Int))
  getOperatorIdByRefreshToken :: Text -> m (Either SomeException (Maybe Int))
