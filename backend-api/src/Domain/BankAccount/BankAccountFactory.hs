module Domain.BankAccount.BankAccountFactory (createBankAccount) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Domain.BankAccount.Entity.BankAccount (BankAccount, mkBankAccount)
import Domain.BankAccount.ValueObject.AccountId (generateAccountId)
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username (mkUsername)
import Domain.ValueError (ValueError)

createBankAccount ::
  Text -> -- Username
  Text -> -- FullName
  Text -> -- Email
  UTCTime -> -- CreatedAt
  IO (Either ValueError BankAccount) -- Updated to use IO
createBankAccount unameTxt fnameTxt emailTxt createdAt = do
  accountId <- generateAccountId -- Still IO
  let result = do
        -- This part uses Either monad
        username <- mkUsername unameTxt
        fullName <- mkFullName fnameTxt
        email <- mkEmail emailTxt
        Right $ mkBankAccount accountId username fullName email createdAt
  pure result
