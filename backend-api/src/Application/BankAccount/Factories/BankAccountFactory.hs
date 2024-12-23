module Application.BankAccount.Factories.BankAccountFactory (createBankAccount) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Domain.BankAccount.Entity.InitialAccount
  ( InitialAccount,
    mkInitialAccount,
  )
import Domain.BankAccount.Repositories.AccountRepository
  ( AccountRepository,
    generateAccountId,
  )
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username (mkUsername)
import Domain.ValueError (ValueError)

createBankAccount ::
  (AccountRepository m, MonadIO m) =>
  Text ->
  Text ->
  Text ->
  UTCTime ->
  m (Either ValueError InitialAccount)
createBankAccount unameTxt fnameTxt emailTxt createdAt = do
  accountId <- generateAccountId
  let result = do
        username <- mkUsername unameTxt
        fullName <- mkFullName fnameTxt
        email <- mkEmail emailTxt
        Right $ mkInitialAccount accountId username fullName email createdAt
  pure result
