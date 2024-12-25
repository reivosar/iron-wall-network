{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.Factories.SuspendAccountFactory (SuspendAccountFactory (..)) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.SuspendAccount (SuspendAccount)
import Domain.ValueError (ValueError)

class SuspendAccountFactory m where
  createSuspendAccount ::
    UUID ->
    UTCTime ->
    Maybe Text ->
    m (Either ValueError SuspendAccount)
