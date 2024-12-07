{-# LANGUAGE FlexibleInstances #-}

module Infrastructure.Repositories.PostgresFundsRepository where

import Control.Exception (SomeException, toException)
import Domain.BankAccount.Entity.Funds (Funds)
import Domain.BankAccount.Repositories.FundsRepository (FundsRepository (..))
import Domain.BankAccount.ValueObject.AccountId (AccountId)

instance FundsRepository IO where
  findById _ = pure $ Right Nothing
  save _ = pure $ Right ()
