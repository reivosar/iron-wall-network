module Infrastructure.Factories.PostgresActiveAccountFactory (createActiveAccount) where

import Application.BankAccount.Factories.ActiveAccountFactory
import Domain.BankAccount.Entity.ActiveAccount
  ( mkActiveAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.BankAccount.ValueObject.AccountPassword (mkAccountPassword)
import Utils.Env (getEnvTextOrThrow)

instance ActiveAccountFactory IO where
  createActiveAccount uuid password activatedAt = do
    let accountId = mkAccountId uuid
    secretKey <- getEnvTextOrThrow "PASSWORD_SECRET_KEY"
    let passwordResult = mkAccountPassword password secretKey
    pure $ do
      accountPassword <- passwordResult
      Right $ mkActiveAccount accountId accountPassword activatedAt
