{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.PostgresSuspendAccountFactory (createSuspendAccount) where

import Application.BankAccount.Factories.SuspendAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.SuspendAccount
  ( mkSuspendAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance (Applicative m, MonadIO m) => SuspendAccountFactory m where
  createSuspendAccount uuid suspendedAt suspensionReason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkSuspendAccount accountId suspendedAt suspensionReason
