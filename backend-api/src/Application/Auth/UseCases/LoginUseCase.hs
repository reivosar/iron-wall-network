{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.LoginUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)

data Input = Input
  { username :: Text,
    password :: Text,
    authKey :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Output = Output
  { accessToken :: Text,
    refreshToken :: Text,
    expiresIn :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

execute :: Input -> IO (Either UseCaseError Output)
execute input = do
  valid <- validateCredentials (username input) (password input)
  if not valid
    then return $ Left (createValidationError "Invalid username or password")
    else do
      tokenResult <- generateTokens (username input)
      case tokenResult of
        Left err -> return $ Left (createSystemError ("Failed to generate tokens: " <> unpack err))
        Right tokens -> return $ Right tokens

validateCredentials :: Text -> Text -> IO Bool
validateCredentials user pass = do
  putStrLn $ "Validating credentials for user: " <> (unpack user)
  return $ user == pack "example_user" && pass == pack "example_password"

generateTokens :: Text -> IO (Either Text Output)
generateTokens user = do
  if user == pack "example_user"
    then
      return $
        Right
          Output
            { accessToken = pack "example_access_token",
              refreshToken = pack "example_refresh_token",
              expiresIn = 3600
            }
    else return $ Left (pack "Token generation failed")
