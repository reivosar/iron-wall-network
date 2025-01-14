{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.BankAccount.EventStoreActiveAccountFactory (createActiveAccount) where

import Application.BankAccount.Factories.ActiveAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.ActiveAccount
  ( mkActiveAccount,
  )
import Domain.BankAccount.Services.BankAccountService (BankAccountService, tryActivate)
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.BankAccount.ValueObject.AccountPassword (mkAccountPassword)
import Infrastructure.Services.Shared.EventStoreEventStatusValidator
import Utils.Env (getEnvTextOrThrow)

instance (BankAccountService m, MonadIO m) => ActiveAccountFactory m where
  createActiveAccount uuid password activatedAt = do
    let accountId = mkAccountId uuid
    activationCheck <- tryActivate accountId
    case activationCheck of
      Left err -> return $ Left err
      Right _ -> do
        secretKey <- getEnvTextOrThrow "PASSWORD_SECRET_KEY"
        let passwordResult = mkAccountPassword password secretKey
        pure $ do
          accountPassword <- passwordResult
          Right $ mkActiveAccount accountId accountPassword activatedAt
