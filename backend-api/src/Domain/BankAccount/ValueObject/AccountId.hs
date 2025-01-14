{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    mkAccountId,
    generateAccountId,
    unwrapAccountId,
  )
where

import Data.UUID (UUID)
import Utils.UUIDGenerator (generateUUID)

newtype AccountId = AccountId {unwrapAccountId :: UUID}
  deriving (Show, Eq)

mkAccountId :: UUID -> AccountId
mkAccountId = AccountId

generateAccountId :: IO AccountId
generateAccountId = AccountId <$> generateUUID
