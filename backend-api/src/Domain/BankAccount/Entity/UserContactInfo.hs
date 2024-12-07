module Domain.BankAccount.Entity.UserContactInfo (UserContactInfo (..), changeEmail, userContactInfoUpserted) where

import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.UserContactInfoUpserted as Event
import Domain.BankAccount.ValueObject.AccountId (AccountId, unwrapAccountId)
import Domain.BankAccount.ValueObject.Email (Email, unwrapEmail)

data UserContactInfo = UserContactInfo
  { accountId :: AccountId,
    email :: Email
  }
  deriving (Show, Eq)

mkUserUserContactInfo :: AccountId -> Email -> UserContactInfo
mkUserUserContactInfo accId email =
  UserContactInfo
    { accountId = accId,
      email = email
    }

changeEmail :: UserContactInfo -> Email -> UTCTime -> UserContactInfo
changeEmail contact email time =
  contact {email = email}

userContactInfoUpserted :: UserContactInfo -> UTCTime -> Event.UserContactInfoUpserted
userContactInfoUpserted contact timestamp =
  Event.UserContactInfoUpserted
    { Event.accountId = unwrapAccountId (accountId contact),
      Event.email = unwrapEmail (email contact),
      Event.updatedAt = timestamp
    }
