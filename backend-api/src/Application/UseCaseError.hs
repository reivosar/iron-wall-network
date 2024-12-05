{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.UseCaseError where

import Data.Aeson (ToJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data UseCaseError
  = ValidationError Text
  | SystemError Text
  | NotFoundError Text
  | AuthenticationError Text
  deriving (Show, Generic, ToJSON)

createValidationError :: String -> UseCaseError
createValidationError msg = ValidationError (pack msg)

createSystemError :: String -> UseCaseError
createSystemError msg = SystemError (pack msg)

createNotFoundError :: String -> UseCaseError
createNotFoundError msg = NotFoundError (pack msg)

createAuthenticationError :: String -> UseCaseError
createAuthenticationError msg = AuthenticationError (pack msg)
