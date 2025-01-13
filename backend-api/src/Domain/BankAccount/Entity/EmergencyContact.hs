{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Entity.EmergencyContact
  ( EmergencyContact (..),
    changeEmergencyContact,
    mkEmergencyContact,
    emergencyContactUpserted,
    parseEmergencyContactFromEvent,
  )
where

import Data.Aeson (decode, encode)
import Data.Text ()
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    mkAccountId,
    unwrapAccountId,
  )
import Domain.BankAccount.ValueObject.FullName
  ( FullName,
    mkFullName,
    unwrapFullName,
  )
import Domain.BankAccount.ValueObject.PhoneNumber
  ( PhoneNumber,
    mkPhoneNumber,
    unwrapPhoneNumber,
  )
import Domain.Event (Event (eventData))
import GHC.Exception (SomeException, toException)
import Utils.Conversions (eitherToMaybe)

data EmergencyContact = EmergencyContact
  { accountId :: AccountId,
    name :: FullName,
    phoneNumber :: PhoneNumber
  }
  deriving (Show, Eq)

mkEmergencyContact :: AccountId -> FullName -> PhoneNumber -> EmergencyContact
mkEmergencyContact accId fllNm phnNmbr =
  EmergencyContact
    { accountId = accId,
      name = fllNm,
      phoneNumber = phnNmbr
    }

changeEmergencyContact :: EmergencyContact -> FullName -> PhoneNumber -> EmergencyContact
changeEmergencyContact contact fllNm phnNmbr =
  mkEmergencyContact
    (accountId contact)
    fllNm
    phnNmbr

emergencyContactUpserted :: EmergencyContact -> UTCTime -> Event.EmergencyContactUpserted
emergencyContactUpserted contact timestamp =
  Event.EmergencyContactUpserted
    { Event.accountId = unwrapAccountId (accountId contact),
      Event.contactName = unwrapFullName (name contact),
      Event.contactPhone = unwrapPhoneNumber (phoneNumber contact),
      Event.updatedAt = timestamp
    }

parseEmergencyContactFromEvent :: Event.EmergencyContactUpserted -> Maybe EmergencyContact
parseEmergencyContactFromEvent event = do
  accId <- pure (mkAccountId (Event.accountId event))
  fllNm <- eitherToMaybe (mkFullName (Event.contactName event))
  phnNmbr <- eitherToMaybe (mkPhoneNumber (Event.contactPhone event))
  return $ mkEmergencyContact accId fllNm phnNmbr
