{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.BankAccount.EventStoreBankAccountFactory (createBankAccount) where

import Application.BankAccount.Factories.BankAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Domain.BankAccount.Entity.InitialAccount (mkInitialAccount)
import Domain.BankAccount.Repositories.AccountRepository (AccountRepository, generateAccountId)
import Domain.BankAccount.Services.BankAccountService (BankAccountService, tryCreate)
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username (mkUsername)

instance (AccountRepository m, BankAccountService m, MonadIO m) => BankAccountFactory m where
  createBankAccount unameTxt fnameTxt emailTxt createdAt = runExceptT $ do
    username <- ExceptT $ return $ mkUsername unameTxt
    ExceptT $ tryCreate username
    accountId <- lift $ generateAccountId
    fullName <- ExceptT $ return $ mkFullName fnameTxt
    email <- ExceptT $ return $ mkEmail emailTxt
    return $ mkInitialAccount accountId username fullName email createdAt
