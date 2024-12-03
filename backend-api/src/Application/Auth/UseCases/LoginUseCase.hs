{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.LoginUseCase where

import qualified Application.Auth.Services.AuthUserDto as AuthUserDto
import qualified Application.Auth.Services.CreateAccessTokenResult as CreateAccessTokenResult
import Application.Auth.Services.TokenService (TokenService, createAccessToken, findUserByUsername)
import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
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
    accessTokenExpiresAt :: UTCTime,
    refreshTokenExpiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

execute :: (TokenService m, Monad m) => Input -> m (Either UseCaseError Output)
execute input = do
  userResult <- findUserByUsername (username input)
  case userResult of
    Left err -> return $ Left (createSystemError ("Error finding user: " <> show err))
    Right Nothing -> return $ Left (createValidationError "Invalid username or password")
    Right (Just user) -> do
      credentialResult <- validateCredentials input user
      case credentialResult of
        Left err -> return $ Left err
        Right () -> do
          tokenResult <- createAccessToken (AuthUserDto.userId user)
          case tokenResult of
            Left err -> return $ Left (createSystemError ("Failed to generate tokens: " <> show err))
            Right tokenData -> return $ Right (convertCreateAccessTokenResultToOutput tokenData)

validateCredentials :: (Monad m) => Input -> AuthUserDto.AuthUserDto -> m (Either UseCaseError ())
validateCredentials input authUser = return $ do
  if password input /= AuthUserDto.passwordHash authUser
    then Left (createValidationError "Invalid username or password")
    else
      if authKey input /= AuthUserDto.authKeyHash authUser
        then Left (createValidationError "Invalid authentication key")
        else Right ()

convertCreateAccessTokenResultToOutput :: CreateAccessTokenResult.CreateAccessTokenResult -> Output
convertCreateAccessTokenResultToOutput result =
  Output
    { accessToken = CreateAccessTokenResult.accessToken result,
      refreshToken = CreateAccessTokenResult.refreshToken result,
      accessTokenExpiresAt = CreateAccessTokenResult.accessTokenExpiresAt result,
      refreshTokenExpiresAt = CreateAccessTokenResult.refreshTokenExpiresAt result
    }
