{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Application.Auth.UseCases.LogoutUseCase (Input (..), execute) where

import Application.Auth.Services.AuthService
  ( AuthService,
    invalidateToken,
  )
import Application.UseCaseError
  ( UseCaseError,
    createSystemError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data Input = Input
  { token :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

execute :: (AuthService m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  result <- invalidateToken (token input)
  case result of
    Left err -> return $ Left $ createSystemError $ "Failed to invalidate the token: " <> pack (show err)
    Right () -> return $ Right ()
