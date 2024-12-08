{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId, generateAccountId, unwrapAccountId) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Domain.ValueError (ValueError (..))
import GHC.Generics (Generic)
import Utils.UUIDGenerator (generateUUID)

newtype AccountId = AccountId {unwrapAccountId :: UUID}
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

mkAccountId :: UUID -> Either ValueError AccountId
mkAccountId uuid =
  if isValidUUID uuid
    then Right $ AccountId uuid
    else Left $ ValueError "Invalid AccountId format. Expected a valid UUID."

generateAccountId :: IO AccountId
generateAccountId = AccountId <$> generateUUID

isValidUUID :: UUID -> Bool
isValidUUID = const True
