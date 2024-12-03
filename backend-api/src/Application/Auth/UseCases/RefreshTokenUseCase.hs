{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.RefreshTokenUseCase where

import qualified Application.Auth.Services.RecreateAccessTokenResult as RecreateAccessTokenResult
import Application.Auth.Services.TokenService (TokenService, findRefreshTokenByRefreshToken, recreateAccessToken)
import qualified Application.Auth.Services.UserRefreshTokenDto as UserRefreshTokenDto
import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Input = Input
  { refreshToken :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Output = Output
  { accessToken :: Text,
    expiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

execute :: (TokenService m, Monad m) => Input -> m (Either UseCaseError Output)
execute input = do
  refreshTokenResult <- findRefreshTokenByRefreshToken (refreshToken input)
  case refreshTokenResult of
    Left err -> return $ Left (createSystemError ("Error finding refresh token: " <> show err))
    Right Nothing -> return $ Left (createValidationError "Invalid or expired refresh token")
    Right (Just storedRefreshToken) -> do
      tokenResult <- recreateAccessToken (UserRefreshTokenDto.userId storedRefreshToken)
      case tokenResult of
        Left err -> return $ Left (createSystemError ("Failed to generate tokens: " <> show err))
        Right tokens -> return $ Right (convertCreateAccessTokenResultToOutput tokens)

convertCreateAccessTokenResultToOutput :: RecreateAccessTokenResult.RecreateAccessTokenResult -> Output
convertCreateAccessTokenResultToOutput result =
  Output
    { accessToken = RecreateAccessTokenResult.accessToken result,
      expiresAt = RecreateAccessTokenResult.accessTokenExpiresAt result
    }
