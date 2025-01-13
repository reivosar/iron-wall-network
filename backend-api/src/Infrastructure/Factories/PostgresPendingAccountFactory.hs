{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.PostgresPendingAccountFactory (createPendingAccount) where

import Application.BankAccount.Factories.PendingAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.PendingAccount
  ( mkPendingAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance (Applicative m, MonadIO m) => PendingAccountFactory m where
  createPendingAccount uuid pendedAt reason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkPendingAccount accountId pendedAt reason
