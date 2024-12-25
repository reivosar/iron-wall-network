{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.Factories.PendingAccountFactory (PendingAccountFactory (..)) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.PendingAccount (PendingAccount)
import Domain.ValueError (ValueError)

class PendingAccountFactory m where
  createPendingAccount ::
    UUID ->
    UTCTime ->
    Maybe Text ->
    m (Either ValueError PendingAccount)
