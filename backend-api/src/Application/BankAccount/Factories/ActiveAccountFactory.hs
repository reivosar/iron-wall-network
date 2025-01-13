{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.Factories.ActiveAccountFactory
  ( ActiveAccountFactory (..),
  )
where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.ActiveAccount (ActiveAccount)
import Domain.Error (DomainError)

class ActiveAccountFactory m where
  createActiveAccount ::
    UUID ->
    Text ->
    UTCTime ->
    m (Either DomainError ActiveAccount)
