module Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId, generateAccountId, unwrapAccountId) where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Domain.ValueError (ValueError (..))

newtype AccountId = AccountId {unwrapAccountId :: UUID}
  deriving (Show, Eq)

mkAccountId :: UUID -> Either ValueError AccountId
mkAccountId uuid =
  if isValidUUID uuid
    then Right $ AccountId uuid
    else Left $ ValueError "Invalid AccountId format. Expected a valid UUID."

generateAccountId :: IO AccountId
generateAccountId = AccountId <$> nextRandom

isValidUUID :: UUID -> Bool
isValidUUID = const True
