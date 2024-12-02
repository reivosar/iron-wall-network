{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.LoginUseCase where

import qualified Application.Auth.Services.AuthUserDto as AuthUserDto
import Application.Auth.Services.TokenService (TokenService, createAccessToken, createRefreshToken, findUserByUsername)
import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Control.Monad (guard, unless)
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

execute :: (TokenService m, Monad m) => Input -> m (Either UseCaseError Output)
execute input = do
  maybeAuthUser <- findUserByUsername (username input)
  case maybeAuthUser of
    Nothing -> return $ Left (createValidationError "Invalid username or password")
    Just authUser -> do
      result <- validateCredentials (input, authUser)
      case result of
        Left err -> return $ Left err
        Right () -> do
          tokenResult <- generateTokens
          case tokenResult of
            Left err -> return $ Left (createSystemError ("Failed to generate tokens: " <> unpack err))
            Right tokens -> do
              createAccessToken (AuthUserDto.userId authUser) (accessToken tokens)
              createRefreshToken (AuthUserDto.userId authUser) (refreshToken tokens)
              return $ Right tokens

validateCredentials :: (Monad m) => (Input, AuthUserDto.AuthUserDto) -> m (Either UseCaseError ())
validateCredentials (input, authUser) = do
  let passwordValid = validatePassword (password input) (AuthUserDto.passwordHash authUser)
      authKeyValid = validateAuthKey (authKey input) (AuthUserDto.authKeyHash authUser)

  if not passwordValid
    then return $ Left (createValidationError "Invalid username or password")
    else
      if not authKeyValid
        then return $ Left (createValidationError "Invalid authentication key")
        else return $ Right ()

validatePassword :: Text -> Text -> Bool
validatePassword input db = input == db

validateAuthKey :: Text -> Text -> Bool
validateAuthKey input db = input == db

generateTokens :: (Monad m) => m (Either Text Output)
generateTokens =
  return $
    Right
      Output
        { accessToken = pack "example_access_token",
          refreshToken = pack "example_refresh_token",
          expiresIn = 3600
        }
