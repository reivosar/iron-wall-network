{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.RefreshTokenUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack, unpack) -- import pack here
import GHC.Generics (Generic)

data Input = Input
  { refreshToken :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Output = Output
  { accessToken :: Text,
    expiresIn :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

execute :: Input -> IO (Either UseCaseError Output)
execute input = do
  valid <- validateRefreshToken (refreshToken input)
  if not valid
    then return $ Left (createValidationError "Invalid refresh token")
    else do
      let output =
            Output
              { accessToken = pack "new_example_access_token",
                expiresIn = 3600
              }
      return $ Right output

validateRefreshToken :: Text -> IO Bool
validateRefreshToken token = do
  putStrLn $ "Validating refresh token: " <> (unpack token)
  return $ token == pack "valid_refresh_token"
