{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.BankAccount.EventStoreBankAccountFactory (createBankAccount) where

import Application.BankAccount.Factories.BankAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.InitialAccount (mkInitialAccount)
import Domain.BankAccount.Services.BankAccountService (BankAccountService, tryCreate)
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username (mkUsername)
import Infrastructure.Repositories.UUIDAccountRepository

instance (BankAccountService m, MonadIO m) => BankAccountFactory m where
  createBankAccount unameTxt fnameTxt emailTxt createdAt = do
    accountId <- generateAccountId
    creationCheck <- tryCreate accountId
    case creationCheck of
      Left err -> return $ Left err
      Right _ -> do
        let result = do
              username <- mkUsername unameTxt
              fullName <- mkFullName fnameTxt
              email <- mkEmail emailTxt
              Right $ mkInitialAccount accountId username fullName email createdAt
        pure result
