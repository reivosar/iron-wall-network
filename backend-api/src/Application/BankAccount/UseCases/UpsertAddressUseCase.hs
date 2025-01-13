{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertAddressUseCase
  ( Input (..),
    execute,
  )
where

import Application.UseCaseError
  ( UseCaseError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.Address (Address, addressUpserted, changeAddress, mkAddress)
import qualified Domain.BankAccount.Events.AddressUpserted as AddressUpserted
import Domain.BankAccount.Repositories.AddressRepository (AddressRepository, findById, save)
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.AddressType (AddressType, textToAddressType)
import Domain.BankAccount.ValueObject.BuildingName (BuildingName, mkBuildingName)
import Domain.BankAccount.ValueObject.City (City, mkCity)
import Domain.BankAccount.ValueObject.PostalCode (PostalCode, mkPostalCode)
import Domain.BankAccount.ValueObject.Prefecture (Prefecture, mkPrefecture)
import Domain.BankAccount.ValueObject.TownArea (TownArea, mkTownArea)
import Domain.DomainEventPublisher (DomainEventPublisher, publishEvent)
import Domain.Error (unwrapDomainError)

data Input = Input
  { accountId :: UUID,
    postalCode :: Text,
    prefecture :: Text,
    city :: Text,
    townArea :: Text,
    buildingName :: Maybe Text,
    addressType :: Text,
    updatedAt :: UTCTime
  }

execute :: (AddressRepository m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let accId = mkAccountId (accountId input)
  let postalCodeResult = mkPostalCode (postalCode input)
  let prefectureResult = mkPrefecture (prefecture input)
  let cityResult = mkCity (city input)
  let townAreaResult = mkTownArea (townArea input)
  let buildingNameResult = traverse mkBuildingName (buildingName input)
  let addressTypeResult = textToAddressType (addressType input)

  case (postalCodeResult, prefectureResult, cityResult, townAreaResult, buildingNameResult, addressTypeResult) of
    (Right postalCodeVo, Right prefectureVo, Right cityVo, Right townAreaVo, Right buildingNameVo, Right addressTypeVo) ->
      findExistingAddress accId >>= processAddress accId postalCodeVo prefectureVo cityVo townAreaVo buildingNameVo addressTypeVo input
    (Left err, _, _, _, _, _) -> return $ Left $ createValidationError $ "Invalid PostalCode: " <> unwrapDomainError err
    (_, Left err, _, _, _, _) -> return $ Left $ createValidationError $ "Invalid Prefecture: " <> unwrapDomainError err
    (_, _, Left err, _, _, _) -> return $ Left $ createValidationError $ "Invalid City: " <> unwrapDomainError err
    (_, _, _, Left err, _, _) -> return $ Left $ createValidationError $ "Invalid TownArea: " <> unwrapDomainError err
    (_, _, _, _, Left err, _) -> return $ Left $ createValidationError $ "Invalid BuildingName: " <> unwrapDomainError err
    (_, _, _, _, _, Left err) -> return $ Left $ createValidationError $ "Invalid AddressType: " <> unwrapDomainError err

findExistingAddress ::
  (AddressRepository m, Monad m) =>
  AccountId ->
  m (Either UseCaseError (Maybe Address))
findExistingAddress accId = do
  result <- findById accId
  return $ case result of
    Left err -> Left $ createValidationError $ "Failed to fetch Address: " <> pack (show err)
    Right address -> Right address

processAddress ::
  (AddressRepository m, DomainEventPublisher m, MonadIO m) =>
  AccountId ->
  PostalCode ->
  Prefecture ->
  City ->
  TownArea ->
  Maybe BuildingName ->
  AddressType ->
  Input ->
  Either UseCaseError (Maybe Address) ->
  m (Either UseCaseError ())
processAddress _ _ _ _ _ _ _ _ (Left err) = return $ Left err
processAddress accId postalCodeVo prefectureVo cityVo townAreaVo buildingNameVo addressTypeVo input (Right Nothing) = do
  let newAddress = mkAddress accId postalCodeVo prefectureVo cityVo townAreaVo buildingNameVo addressTypeVo
  updateAddressAndPublishEvent input newAddress
processAddress _ postalCodeVo prefectureVo cityVo townAreaVo buildingNameVo addressTypeVo input (Right (Just existingAddress)) = do
  let updatedAddress = changeAddress existingAddress postalCodeVo prefectureVo cityVo townAreaVo buildingNameVo addressTypeVo
  updateAddressAndPublishEvent input updatedAddress

updateAddressAndPublishEvent ::
  (AddressRepository m, DomainEventPublisher m, MonadIO m) =>
  Input ->
  Address ->
  m (Either UseCaseError ())
updateAddressAndPublishEvent input address = do
  saveResult <- save address
  case saveResult of
    Left err -> return $ Left $ createValidationError $ "Failed to save Address: " <> pack (show err)
    Right _ -> publishAddressEvent input address

publishAddressEvent ::
  (DomainEventPublisher m, Monad m) =>
  Input ->
  Address ->
  m (Either UseCaseError ())
publishAddressEvent input address = do
  let event = addressUpserted address (updatedAt input)

  result <-
    publishEvent
      (accountId input)
      (aggregateTypeToText Account)
      AddressUpserted.eventName
      "system"
      event
      Nothing

  return $ case result of
    Left err -> Left $ mapDomainEventErrorToUseCaseError err
    Right _ -> Right ()
