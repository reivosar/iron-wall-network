{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.ValidateTokenUseCase
  ( Input (..),
    execute,
    mapAuthServiceErrorToUseCaseError,
  )
where

import Application.Auth.Services.AuthService
  ( AuthService,
    AuthServiceError (..),
    validateToken,
  )
import Application.UseCaseError
  ( UseCaseError,
    createAuthenticationError,
    createValidationError,
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Input = Input
  { accessToken :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

execute :: (AuthService m, Monad m) => Input -> m (Either UseCaseError ())
execute input = do
  validationResult <- validateToken (accessToken input)
  case validationResult of
    Left err -> return $ Left (mapAuthServiceErrorToUseCaseError err)
    Right () -> return $ Right ()

mapAuthServiceErrorToUseCaseError :: AuthServiceError -> UseCaseError
mapAuthServiceErrorToUseCaseError err = case err of
  TokenNotFoundError msg -> createAuthenticationError msg
  TokenExpiredError msg -> createAuthenticationError msg
  _ -> createValidationError "An unexpected error occurred while validating the token"
