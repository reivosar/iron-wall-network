{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.Entity.Address
  ( Address (..),
    mkAddress,
    changeBuildingName,
    addressUpserted,
  )
where

import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.AddressUpserted as Event
import Domain.BankAccount.ValueObject.AccountId (AccountId, unwrapAccountId)
import Domain.BankAccount.ValueObject.AddressType (AddressType, addressTypeToText)
import Domain.BankAccount.ValueObject.BuildingName (BuildingName, unwrapBuildingName)
import Domain.BankAccount.ValueObject.City (City, unwrapCity)
import Domain.BankAccount.ValueObject.PostalCode (PostalCode, unwrapPostalCode)
import Domain.BankAccount.ValueObject.Prefecture (Prefecture, unwrapPrefecture)
import Domain.BankAccount.ValueObject.TownArea (TownArea, unwrapTownArea)

data Address = Address
  { accountId :: AccountId,
    postalCode :: PostalCode,
    prefecture :: Prefecture,
    city :: City,
    townArea :: TownArea,
    buildingName :: Maybe BuildingName,
    addressType :: AddressType
  }
  deriving (Show, Eq)

mkAddress ::
  AccountId -> PostalCode -> Prefecture -> City -> TownArea -> Maybe BuildingName -> AddressType -> Address
mkAddress accId pc pref cty ta bld addrType =
  Address
    { accountId = accId,
      postalCode = pc,
      prefecture = pref,
      city = cty,
      townArea = ta,
      buildingName = bld,
      addressType = addrType
    }

changeBuildingName :: Address -> Maybe BuildingName -> Address
changeBuildingName addrss newBuildingName =
  addrss {buildingName = newBuildingName}

addressUpserted :: Address -> UTCTime -> Event.AddressUpserted
addressUpserted addrss timestamp =
  Event.AddressUpserted
    { Event.accountId = unwrapAccountId (accountId addrss),
      Event.postalCode = unwrapPostalCode (postalCode addrss),
      Event.prefecture = unwrapPrefecture (prefecture addrss),
      Event.city = unwrapCity (city addrss),
      Event.townArea = unwrapTownArea (townArea addrss),
      Event.buildingName = fmap unwrapBuildingName (buildingName addrss),
      Event.addressType = addressTypeToText (addressType addrss),
      Event.updatedAt = timestamp
    }
