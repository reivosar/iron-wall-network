{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.LogoutUseCase where

import Application.UseCaseError (UseCaseError, createSystemError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import GHC.Generics (Generic)

data Input = Input
  { token :: Text
  }
  deriving (Show, Generic)

execute :: (MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  success <- liftIO $ invalidateToken (token input)
  if success
    then return $ Right ()
    else return $ Left $ createSystemError "Failed to invalidate the token"

invalidateToken :: Text -> IO Bool
invalidateToken token = do
  putStrLn $ "Invalidating token: " <> show token
  return True
