module Domain.BankAccount.ValueObject.Balance (Balance, mkBalance, unwrapBalance, addBalance, subtractBalance) where

import Domain.ValueError (ValueError (..))

newtype Balance = Balance {unwrapBalance :: Double}
  deriving (Show, Eq, Ord)

mkBalance :: Double -> Either ValueError Balance
mkBalance amount
  | amount < 0 = Left $ ValueError "Balance cannot be negative."
  | otherwise = Right $ Balance amount

addBalance :: Balance -> Double -> Either ValueError Balance
addBalance (Balance current) amount
  | amount < 0 = Left $ ValueError "Cannot add a negative amount to balance."
  | otherwise = mkBalance (current + amount)

subtractBalance :: Balance -> Double -> Either ValueError Balance
subtractBalance (Balance current) amount
  | amount < 0 = Left $ ValueError "Cannot subtract a negative amount from balance."
  | current < amount = Left $ ValueError "Insufficient balance for this transaction."
  | otherwise = mkBalance (current - amount)