{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.PostgresCloseAccountFactory (createCloseAccount) where

import Application.BankAccount.Factories.CloseAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.CloseAccount
  ( mkCloseAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance (Applicative m, MonadIO m) => CloseAccountFactory m where
  createCloseAccount uuid closedAt closureReason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkCloseAccount accountId closedAt closureReason
