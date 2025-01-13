{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.Factories.CloseAccountFactory (CloseAccountFactory (..)) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.CloseAccount (CloseAccount)
import Domain.Error (DomainError)

class CloseAccountFactory m where
  createCloseAccount ::
    UUID ->
    UTCTime ->
    Maybe Text ->
    m (Either DomainError CloseAccount)
