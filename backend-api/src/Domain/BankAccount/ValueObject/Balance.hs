{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.Balance
  ( Balance,
    mkBalance,
    unwrapBalance,
    addBalance,
    subtractBalance,
  )
where

import Domain.Error (DomainError, mkDomainError)

newtype Balance = Balance {unwrapBalance :: Double}
  deriving (Show, Eq)

mkBalance :: Double -> Either DomainError Balance
mkBalance amount
  | amount < 0 = Left $ mkDomainError "Balance cannot be negative."
  | otherwise = Right $ Balance amount

addBalance :: Balance -> Double -> Either DomainError Balance
addBalance (Balance current) amount
  | amount < 0 = Left $ mkDomainError "Cannot add a negative amount to balance."
  | otherwise = mkBalance (current + amount)

subtractBalance :: Balance -> Double -> Either DomainError Balance
subtractBalance (Balance current) amount
  | amount < 0 = Left $ mkDomainError "Cannot subtract a negative amount from balance."
  | current < amount = Left $ mkDomainError "Insufficient balance for this transaction."
  | otherwise = mkBalance (current - amount)
