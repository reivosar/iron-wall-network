module Domain.BankAccount.Entity.UserContactInfo
  ( UserContactInfo (..),
    changeEmail,
    mkUserUserContactInfo,
    userContactInfoUpserted,
  )
where

import Data.Text ()
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.UserContactInfoUpserted as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )
import Domain.BankAccount.ValueObject.Email
  ( Email,
    unwrapEmail,
  )

data UserContactInfo = UserContactInfo
  { accountId :: AccountId,
    email :: Email
  }
  deriving (Show, Eq)

mkUserUserContactInfo :: AccountId -> Email -> UserContactInfo
mkUserUserContactInfo accId eml =
  UserContactInfo
    { accountId = accId,
      email = eml
    }

changeEmail :: UserContactInfo -> Email -> UserContactInfo
changeEmail contact eml =
  contact {email = eml}

userContactInfoUpserted :: UserContactInfo -> UTCTime -> Event.UserContactInfoUpserted
userContactInfoUpserted contact timestamp =
  Event.UserContactInfoUpserted
    { Event.accountId = unwrapAccountId (accountId contact),
      Event.email = unwrapEmail (email contact),
      Event.updatedAt = timestamp
    }
