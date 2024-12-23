module Application.BankAccount.Factories.ActiveAccountFactory (createActiveAccount) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.ActiveAccount
  ( ActiveAccount,
    mkActiveAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.BankAccount.ValueObject.AccountPassword (mkAccountPassword)
import Domain.ValueError (ValueError)
import Utils.Env (getEnvTextOrThrow)

createActiveAccount ::
  UUID ->
  Text ->
  UTCTime ->
  IO (Either ValueError ActiveAccount)
createActiveAccount uuid password activatedAt = do
  let accountId = mkAccountId uuid
  secretKey <- getEnvTextOrThrow "PASSWORD_SECRET_KEY"
  let passwordResult = mkAccountPassword password secretKey
  pure $ do
    accountPassword <- passwordResult
    Right $ mkActiveAccount accountId accountPassword activatedAt
