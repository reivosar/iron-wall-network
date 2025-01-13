{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Entity.Funds
  ( Funds (..),
    mkFunds,
    depositFunds,
    addBalance,
    subtractBalance,
    withdrawFunds,
    parseFundsFromDepositedEvent,
    parseFundsFromWithdrawnEvent,
  )
where

import Data.Aeson (decode, encode)
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import qualified Domain.BankAccount.Events.FundsWithdrawn as FundsWithdrawn
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    mkAccountId,
    unwrapAccountId,
  )
import qualified Domain.BankAccount.ValueObject.Balance as Balance
import Domain.Error (DomainError (..))
import Domain.Event (Event (eventData))
import Utils.Conversions (eitherToMaybe)

data Funds = Funds
  { accountId :: AccountId,
    balance :: Balance.Balance
  }
  deriving (Show, Eq)

mkFunds :: AccountId -> Balance.Balance -> Either DomainError Funds
mkFunds accId initialBalance =
  Right $
    Funds
      { accountId = accId,
        balance = initialBalance
      }

addBalance :: Funds -> Double -> Either DomainError Funds
addBalance funds amount = do
  updatedBalance <- Balance.addBalance (balance funds) amount
  mkFunds (accountId funds) updatedBalance

subtractBalance :: Funds -> Double -> Either DomainError Funds
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

parseFundsFromDepositedEvent :: FundsDeposited.FundsDeposited -> Maybe Funds
parseFundsFromDepositedEvent depositedEvent = do
  accId <- Just (mkAccountId (FundsDeposited.accountId depositedEvent))
  balance <- eitherToMaybe (Balance.mkBalance (FundsDeposited.totalBalance depositedEvent))
  eitherToMaybe $ mkFunds accId balance

parseFundsFromWithdrawnEvent :: FundsWithdrawn.FundsWithdrawn -> Maybe Funds
parseFundsFromWithdrawnEvent withdrawnEvent = do
  accId <- Just (mkAccountId (FundsWithdrawn.accountId withdrawnEvent))
  balance <- eitherToMaybe (Balance.mkBalance (FundsWithdrawn.totalBalance withdrawnEvent))
  eitherToMaybe $ mkFunds accId balance
