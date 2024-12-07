module Domain.BankAccount.Entity.Funds (Funds (..), mkFunds, depositFunds, addBalance, subtractBalance, withdrawFunds) where

import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import qualified Domain.BankAccount.Events.FundsWithdrawn as FundsWithdrawn
import Domain.BankAccount.ValueObject.AccountId (AccountId, unwrapAccountId)
import qualified Domain.BankAccount.ValueObject.Balance as Balance
import Domain.ValueError (ValueError (..))

data Funds = Funds
  { accountId :: AccountId,
    balance :: Balance.Balance
  }
  deriving (Show, Eq)

mkFunds :: AccountId -> Balance.Balance -> Either ValueError Funds
mkFunds accId initialBalance =
  Right $
    Funds
      { accountId = accId,
        balance = initialBalance
      }

addBalance :: Funds -> Double -> Either ValueError Funds
addBalance funds amount = do
  updatedBalance <- Balance.addBalance (balance funds) amount
  Right funds {balance = updatedBalance}

subtractBalance :: Funds -> Double -> Either ValueError Funds
subtractBalance funds amount = do
  updatedBalance <- Balance.subtractBalance (balance funds) amount
  Right funds {balance = updatedBalance}

depositFunds :: Funds -> UTCTime -> FundsDeposited.FundsDeposited
depositFunds funds timestamp =
  FundsDeposited.FundsDeposited (unwrapAccountId (accountId funds)) (Balance.unwrapBalance (balance funds)) timestamp

withdrawFunds :: Funds -> UTCTime -> FundsWithdrawn.FundsWithdrawn
withdrawFunds funds timestamp =
  FundsWithdrawn.FundsWithdrawn (unwrapAccountId (accountId funds)) (Balance.unwrapBalance (balance funds)) timestamp
