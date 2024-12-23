{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.UseCaseError
  ( UseCaseError (..),
    createAuthenticationError,
    createNotFoundError,
    createSystemError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text, pack)
import Domain.DomainEventPublisher (DomainEventError (..))
import GHC.Generics (Generic)

data UseCaseError
  = ValidationError Text
  | SystemError Text
  | NotFoundError Text
  | AuthenticationError Text
  deriving (Show, Generic, ToJSON)

createValidationError :: Text -> UseCaseError
createValidationError msg = ValidationError msg

createSystemError :: Text -> UseCaseError
createSystemError msg = SystemError msg

createNotFoundError :: Text -> UseCaseError
createNotFoundError msg = NotFoundError msg

createAuthenticationError :: Text -> UseCaseError
createAuthenticationError msg = AuthenticationError msg

mapDomainEventErrorToUseCaseError :: DomainEventError -> UseCaseError
mapDomainEventErrorToUseCaseError (PublishEventFailed msg) =
  createSystemError ("Event publishing failed: " <> msg)
mapDomainEventErrorToUseCaseError (UnexpectedError msg) =
  createSystemError ("Unexpected error: " <> msg)
