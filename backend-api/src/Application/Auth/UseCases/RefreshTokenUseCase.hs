{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.RefreshTokenUseCase where

import Application.Auth.Services.TokenService (TokenService, createAccessToken, findRefreshTokenByRefreshToken)
import qualified Application.Auth.Services.UserRefreshTokenDto as UserRefreshTokenDto
import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack, unpack)
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

execute :: (TokenService m, Monad m) => Input -> m (Either UseCaseError Output)
execute input = do
  maybeStoredRefreshToken <- findRefreshTokenByRefreshToken (refreshToken input)

  case maybeStoredRefreshToken of
    Nothing -> return $ Left (createValidationError "Invalid or expired refresh token")
    Just storedRefreshToken -> do
      tokenResult <- generateTokens
      case tokenResult of
        Left err -> return $ Left (createSystemError ("Failed to generate tokens: " <> unpack err))
        Right tokens -> do
          createAccessToken (UserRefreshTokenDto.userId storedRefreshToken) (accessToken tokens)
          return $ Right tokens

generateTokens :: (Monad m) => m (Either Text Output)
generateTokens =
  return $
    Right
      Output
        { accessToken = pack "example_access_token",
          expiresIn = 3600
        }
