module Domain.BankAccount.ActiveAccountFactory (createActiveAccount) where

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

createActiveAccount ::
  UUID ->
  Text ->
  UTCTime ->
  IO (Either ValueError ActiveAccount)
createActiveAccount uuid password activatedAt = do
  let accountIdResult = mkAccountId uuid
  passwordResult <- mkAccountPassword password
  pure $ do
    accountId <- accountIdResult
    accountPassword <- passwordResult
    Right $ mkActiveAccount accountId accountPassword activatedAt
