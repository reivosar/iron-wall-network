{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.Factories.ApproveAccountFactory (ApproveAccountFactory (..)) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.ApproveAccount (ApproveAccount)
import Domain.ValueError (ValueError)

class ApproveAccountFactory m where
  createApproveAccount ::
    UUID ->
    UTCTime ->
    Maybe Text ->
    m (Either ValueError ApproveAccount)
