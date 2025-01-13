{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.Entity.EmailContact
  ( EmailContact (..),
    changeEmailContact,
    mkEmailContact,
    emailContactUpserted,
    parseEmailContactFromEvent,
  )
where

import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.EmailContactUpserted as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    mkAccountId,
    unwrapAccountId,
  )
import Domain.BankAccount.ValueObject.Email
  ( Email,
    mkEmail,
    unwrapEmail,
  )
import Utils.Conversions (eitherToMaybe)

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

changeEmailContact :: EmailContact -> Email -> EmailContact
changeEmailContact contact eml =
  mkEmailContact
    (accountId contact)
    eml

emailContactUpserted :: EmailContact -> UTCTime -> Event.EmailContactUpserted
emailContactUpserted contact timestamp =
  Event.EmailContactUpserted
    { Event.accountId = unwrapAccountId (accountId contact),
      Event.email = unwrapEmail (email contact),
      Event.updatedAt = timestamp
    }

parseEmailContactFromEvent :: Event.EmailContactUpserted -> Maybe EmailContact
parseEmailContactFromEvent event = do
  accId <- pure (mkAccountId (Event.accountId event))
  eml <- eitherToMaybe (mkEmail (Event.email event))
  return $ mkEmailContact accId eml
