{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.Factories.SuspendAccountFactory (SuspendAccountFactory (..)) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.SuspendAccount (SuspendAccount)
import Domain.Error (DomainError)

class SuspendAccountFactory m where
  createSuspendAccount ::
    UUID ->
    UTCTime ->
    Maybe Text ->
    m (Either DomainError SuspendAccount)
