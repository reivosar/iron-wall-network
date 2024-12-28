module Domain.BankAccount.Entity.EmailContact
  ( EmailContact (..),
    changeEmail,
    mkEmailContact,
    emailUpserted,
  )
where

import Data.Text ()
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.EmailContactUpserted as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )
import Domain.BankAccount.ValueObject.Email
  ( Email,
    unwrapEmail,
  )

data EmailContact = EmailContact
  { accountId :: AccountId,
    email :: Email
  }
  deriving (Show, Eq)

mkEmailContact :: AccountId -> Email -> EmailContact
mkEmailContact accId eml =
  EmailContact
    { accountId = accId,
      email = eml
    }

changeEmail :: EmailContact -> Email -> EmailContact
changeEmail contact eml =
  mkEmailContact
    (accountId contact)
    eml

emailUpserted :: EmailContact -> UTCTime -> Event.EmailContactUpserted
emailUpserted contact timestamp =
  Event.EmailContactUpserted
    { Event.accountId = unwrapAccountId (accountId contact),
      Event.email = unwrapEmail (email contact),
      Event.updatedAt = timestamp
    }
