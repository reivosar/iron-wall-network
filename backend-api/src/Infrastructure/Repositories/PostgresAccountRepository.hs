{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repositories.PostgresAccountRepository
  ( generateAccountId,
  )
where

import Domain.BankAccount.Repositories.AccountRepository (AccountRepository (..))
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Utils.UUIDGenerator (generateUUID)

instance AccountRepository IO where
  generateAccountId = mkAccountId <$> generateUUID
