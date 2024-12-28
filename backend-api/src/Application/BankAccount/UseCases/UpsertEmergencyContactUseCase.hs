{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertEmergencyContactUseCase
  ( Input (..),
    execute,
  )
where

import Application.UseCaseError
  ( UseCaseError,
    createSystemError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.EmergencyContact (EmergencyContact, changeEmergencyContact, emergencyContactUpserted, mkEmergencyContact)
import Domain.BankAccount.Repositories.EmergencyContactRepository (EmergencyContactRepository, findById, save)
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.FullName (FullName, mkFullName)
import Domain.BankAccount.ValueObject.PhoneNumber (PhoneNumber, mkPhoneNumber)
import Domain.DomainEventPublisher (DomainEventPublisher, publishEvent)
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    contactName :: Text,
    contactPhone :: Text,
    updatedAt :: UTCTime
  }

execute :: (EmergencyContactRepository m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let accId = mkAccountId (accountId input)
  let contactNameResult = mkFullName (contactName input)
  let contactPhoneResult = mkPhoneNumber (contactPhone input)

  case (contactNameResult, contactPhoneResult) of
    (Right contactNameVo, Right contactPhoneVo) -> do
      findEmergencyContact accId >>= processEmergencyContact accId input contactNameVo contactPhoneVo
    (Left err, _) -> return $ Left $ createValidationError $ "Invalid FullName: " <> unwrapValueError err
    (_, Left err) -> return $ Left $ createValidationError $ "Invalid PhoneNumber: " <> unwrapValueError err

findEmergencyContact ::
  (EmergencyContactRepository m, Monad m) =>
  AccountId ->
  m (Either UseCaseError (Maybe EmergencyContact))
findEmergencyContact accId = do
  result <- findById accId
  return $ case result of
    Left err -> Left $ createSystemError $ "Failed to fetch EmergencyContact: " <> pack (show err)
    Right contact -> Right contact

processEmergencyContact ::
  (EmergencyContactRepository m, DomainEventPublisher m, MonadIO m) =>
  AccountId ->
  Input ->
  FullName ->
  PhoneNumber ->
  Either UseCaseError (Maybe EmergencyContact) ->
  m (Either UseCaseError ())
processEmergencyContact _ _ _ _ (Left err) = return $ Left err
processEmergencyContact accId input newFullName newPhoneNumber (Right Nothing) = do
  let newEmergencyContact = mkEmergencyContact accId newFullName newPhoneNumber
  updateEmergencyContactAndPublishEvent input newEmergencyContact
processEmergencyContact _ input newFullName newPhoneNumber (Right (Just existingEmergencyContact)) = do
  let updatedEmergencyContact = changeEmergencyContact existingEmergencyContact newFullName newPhoneNumber
  updateEmergencyContactAndPublishEvent input updatedEmergencyContact

updateEmergencyContactAndPublishEvent ::
  (EmergencyContactRepository m, DomainEventPublisher m, MonadIO m) =>
  Input ->
  EmergencyContact ->
  m (Either UseCaseError ())
updateEmergencyContactAndPublishEvent input emergencyContact = do
  saveResult <- save emergencyContact
  case saveResult of
    Left err -> return $ Left $ createValidationError $ "Failed to save EmergencyContact: " <> pack (show err)
    Right _ -> publishEmergencyContactEvent input emergencyContact

publishEmergencyContactEvent ::
  (DomainEventPublisher m, Monad m) =>
  Input ->
  EmergencyContact ->
  m (Either UseCaseError ())
publishEmergencyContactEvent input emergencyContact = do
  let event = emergencyContactUpserted emergencyContact (updatedAt input)
  result <- publishEvent (accountId input) "account" "EmergencyContactUpserted" "system" event Nothing
  return $ case result of
    Left err -> Left $ mapDomainEventErrorToUseCaseError err
    Right _ -> Right ()
