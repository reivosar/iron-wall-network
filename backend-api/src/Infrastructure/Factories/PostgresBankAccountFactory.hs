{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.PostgresBankAccountFactory (createBankAccount) where

import Application.BankAccount.Factories.BankAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.InitialAccount
  ( mkInitialAccount,
  )
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username (mkUsername)
import Infrastructure.Repositories.UUIDAccountRepository

instance (Applicative m, MonadIO m) => BankAccountFactory m where
  createBankAccount unameTxt fnameTxt emailTxt createdAt = do
    accountId <- generateAccountId
    let result = do
          username <- mkUsername unameTxt
          fullName <- mkFullName fnameTxt
          email <- mkEmail emailTxt
          Right $ mkInitialAccount accountId username fullName email createdAt
    pure result
