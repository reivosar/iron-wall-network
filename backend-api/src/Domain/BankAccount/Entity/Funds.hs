{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Entity.Funds (Funds (..), mkFunds, depositFunds, addBalance, subtractBalance, withdrawFunds) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import qualified Domain.BankAccount.Events.FundsWithdrawn as FundsWithdrawn
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )
import qualified Domain.BankAccount.ValueObject.Balance as Balance
import Domain.ValueError (ValueError (..))
import GHC.Generics (Generic)

data Funds = Funds
  { accountId :: AccountId,
    balance :: Balance.Balance
  }
  deriving (Show, Generic, FromJSON, ToJSON)

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
  mkFunds (accountId funds) updatedBalance

subtractBalance :: Funds -> Double -> Either ValueError Funds
subtractBalance funds amount = do
  updatedBalance <- Balance.subtractBalance (balance funds) amount
  mkFunds (accountId funds) updatedBalance

depositFunds :: Funds -> Double -> UTCTime -> FundsDeposited.FundsDeposited
depositFunds funds amount timestamp =
  FundsDeposited.FundsDeposited
    { FundsDeposited.accountId = unwrapAccountId (accountId funds),
      FundsDeposited.amount = amount,
      FundsDeposited.totalBalance = Balance.unwrapBalance (balance funds),
      FundsDeposited.depositedAt = timestamp
    }

withdrawFunds :: Funds -> Double -> UTCTime -> FundsWithdrawn.FundsWithdrawn
withdrawFunds funds amount timestamp =
  FundsWithdrawn.FundsWithdrawn
    { FundsWithdrawn.accountId = unwrapAccountId (accountId funds),
      FundsWithdrawn.amount = amount,
      FundsWithdrawn.totalBalance = Balance.unwrapBalance (balance funds),
      FundsWithdrawn.withdrawnAt = timestamp
    }
