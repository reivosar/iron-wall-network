{-# LANGUAGE DeriveGeneric #-}

module Domain.Error
  ( DomainError (..),
    mkDomainError,
    unwrapDomainError,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

data DomainError
  = ValidationError Text
  | StateTransitionError Text
  | MissingEventError Text
  | BusinessRuleViolation Text
  | InternalError Text
  deriving (Show, Eq, Generic)

mkDomainError :: Text -> DomainError
mkDomainError = InternalError

unwrapDomainError :: DomainError -> Text
unwrapDomainError (ValidationError msg) = msg
unwrapDomainError (StateTransitionError msg) = msg
unwrapDomainError (MissingEventError msg) = msg
unwrapDomainError (BusinessRuleViolation msg) = msg
unwrapDomainError (InternalError msg) = msg
