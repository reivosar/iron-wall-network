{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.Entity.Address
  ( Address (..),
    mkAddress,
    changeAddress,
    addressUpserted,
    parseAddressFromEvent,
  )
where

import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.AddressUpserted as Event
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId, unwrapAccountId)
import Domain.BankAccount.ValueObject.AddressType (AddressType, addressTypeToText, textToAddressType)
import Domain.BankAccount.ValueObject.BuildingName (BuildingName, mkBuildingName, unwrapBuildingName)
import Domain.BankAccount.ValueObject.City (City, mkCity, unwrapCity)
import Domain.BankAccount.ValueObject.PostalCode (PostalCode, mkPostalCode, unwrapPostalCode)
import Domain.BankAccount.ValueObject.Prefecture (Prefecture, mkPrefecture, unwrapPrefecture)
import Domain.BankAccount.ValueObject.TownArea (TownArea, mkTownArea, unwrapTownArea)
import Utils.Conversions (eitherToMaybe)

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

changeAddress :: Address -> PostalCode -> Prefecture -> City -> TownArea -> Maybe BuildingName -> AddressType -> Address
changeAddress addrss postalCodeVo prefectureVo cityVo townAreaVo buildingNameVo addressTypeVo =
  mkAddress
    (accountId addrss)
    postalCodeVo
    prefectureVo
    cityVo
    townAreaVo
    buildingNameVo
    addressTypeVo

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

parseAddressFromEvent :: Event.AddressUpserted -> Maybe Address
parseAddressFromEvent decodedEvent = do
  accId <- pure (mkAccountId (Event.accountId decodedEvent))
  pc <- eitherToMaybe $ mkPostalCode (Event.postalCode decodedEvent)
  pref <- eitherToMaybe $ mkPrefecture (Event.prefecture decodedEvent)
  ct <- eitherToMaybe $ mkCity (Event.city decodedEvent)
  ta <- eitherToMaybe $ mkTownArea (Event.townArea decodedEvent)
  let bldName = Event.buildingName decodedEvent >>= eitherToMaybe . mkBuildingName
  addrType <- eitherToMaybe $ textToAddressType (Event.addressType decodedEvent)
  return $ mkAddress accId pc pref ct ta bldName addrType
