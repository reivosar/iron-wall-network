{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.Factories.BankAccountFactory (BankAccountFactory (..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Domain.BankAccount.Entity.InitialAccount (InitialAccount)
import Domain.BankAccount.Repositories.AccountRepository
import Domain.ValueError (ValueError)

class (AccountRepository m, MonadIO m) => BankAccountFactory m where
  createBankAccount ::
    Text ->
    Text ->
    Text ->
    UTCTime ->
    m (Either ValueError InitialAccount)
