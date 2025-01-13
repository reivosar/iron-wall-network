{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.PostgresApproveAccountFactory (createApproveAccount) where

import Application.BankAccount.Factories.ApproveAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.ApproveAccount
  ( mkApproveAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance (Applicative m, MonadIO m) => ApproveAccountFactory m where
  createApproveAccount uuid approvedAt approvalNotes = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkApproveAccount accountId approvedAt approvalNotes
