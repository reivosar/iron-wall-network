{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.UUIDAccountRepository
  ( generateAccountId,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Repositories.AccountRepository (AccountRepository (..))
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Utils.UUIDGenerator (generateUUID)

instance (Applicative m, MonadIO m) => AccountRepository m where
  generateAccountId = mkAccountId <$> generateUUID
