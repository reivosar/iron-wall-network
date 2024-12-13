{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.LoginUseCase where

import Application.Auth.Services.AuthService
  ( AuthService,
    createAccessToken,
    findUserByUsername,
  )
import qualified Application.Auth.Services.AuthUserDto as AuthUserDto
import qualified Application.Auth.Services.CreateAccessTokenResult as CreateAccessTokenResult
import Application.UseCaseError
  ( UseCaseError,
    createSystemError,
    createValidationError,
  )
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Text
  ( Text,
    unpack,
  )
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Utils.Env
import Utils.HashGenerator

data Input = Input
  { userName :: Text,
    password :: Text,
    authKey :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Output = Output
  { accessToken :: Text,
    refreshToken :: Text,
    issuedAt :: UTCTime,
    accessTokenExpiresAt :: UTCTime,
    refreshTokenExpiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

execute :: (AuthService m, MonadIO m) => Input -> m (Either UseCaseError Output)
execute input = do
  userResult <- findUserByUsername (userName input)
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

validateCredentials :: (MonadIO m) => Input -> AuthUserDto.AuthUserDto -> m (Either UseCaseError ())
validateCredentials input authUser = do
  passwordSecret <- getEnvTextOrThrow "PASSWORD_SECRET_KEY"
  authKeySecret <- getEnvTextOrThrow "AUTH_KEY_SECRET_KEY"

  let hashedPassword = generateHMAC (password input) passwordSecret
  let hashedAuthKey = generateHMAC (authKey input) authKeySecret

  return $
    if hashedPassword /= AuthUserDto.passwordHash authUser
      then Left (createValidationError "Invalid username or password")
      else
        if hashedAuthKey /= AuthUserDto.authKeyHash authUser
          then Left (createValidationError "Invalid authentication key")
          else Right ()

convertCreateAccessTokenResultToOutput :: CreateAccessTokenResult.CreateAccessTokenResult -> Output
convertCreateAccessTokenResultToOutput result =
  Output
    { accessToken = CreateAccessTokenResult.accessToken result,
      refreshToken = CreateAccessTokenResult.refreshToken result,
      issuedAt = CreateAccessTokenResult.issuedAt result,
      accessTokenExpiresAt = CreateAccessTokenResult.accessTokenExpiresAt result,
      refreshTokenExpiresAt = CreateAccessTokenResult.refreshTokenExpiresAt result
    }
