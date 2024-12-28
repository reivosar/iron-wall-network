{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertPhoneNumberContactUseCase
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
import Domain.BankAccount.Entity.PhoneNumberContact (PhoneNumberContact, changePhoneNumber, mkPhoneNumberContact, phoneNumberUpserted)
import Domain.BankAccount.Repositories.PhoneNumberRepository (PhoneNumberRepository, findById, save)
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.PhoneNumber (PhoneNumber, mkPhoneNumber)
import Domain.BankAccount.ValueObject.PhoneType (PhoneType, textToPhoneType)
import Domain.DomainEventPublisher (DomainEventPublisher, publishEvent)
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    phoneNumber :: Text,
    phoneType :: Text,
    updatedAt :: UTCTime
  }

execute :: (PhoneNumberRepository m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let accId = mkAccountId (accountId input)
  let phoneNumberResult = mkPhoneNumber (phoneNumber input)
  let phoneTypeResult = textToPhoneType (phoneType input)

  case (phoneNumberResult, phoneTypeResult) of
    (Right phoneNumberVo, Right phoneTypeVo) ->
      findPhoneNumber accId >>= processPhoneNumber input phoneNumberVo phoneTypeVo
    (Left err, _) -> return $ Left $ createValidationError $ "Invalid PhoneNumber: " <> unwrapValueError err
    (_, Left err) -> return $ Left $ createValidationError $ "Invalid PhoneType: " <> unwrapValueError err

findPhoneNumber :: (PhoneNumberRepository m, Monad m) => AccountId -> m (Either UseCaseError (Maybe PhoneNumberContact))
findPhoneNumber accId = do
  result <- findById accId
  return $ case result of
    Left err -> Left $ createSystemError $ "Failed to fetch phoneNumber: " <> pack (show err)
    Right phoneNumberContact -> Right phoneNumberContact

processPhoneNumber ::
  (PhoneNumberRepository m, DomainEventPublisher m, MonadIO m) =>
  Input ->
  PhoneNumber ->
  PhoneType ->
  Either UseCaseError (Maybe PhoneNumberContact) ->
  m (Either UseCaseError ())
processPhoneNumber _ _ _ (Left err) = return $ Left err
processPhoneNumber input newPhoneNumber newPhoneType (Right Nothing) = do
  let newPhoneNumberContact = mkPhoneNumberContact (mkAccountId (accountId input)) newPhoneType newPhoneNumber
  updatePhoneNumberAndPublishEvent input newPhoneNumber newPhoneType newPhoneNumberContact
processPhoneNumber input newPhoneNumber newPhoneType (Right (Just existingPhoneNumberContact)) =
  updatePhoneNumberAndPublishEvent input newPhoneNumber newPhoneType existingPhoneNumberContact

updatePhoneNumberAndPublishEvent ::
  (PhoneNumberRepository m, DomainEventPublisher m, MonadIO m) =>
  Input ->
  PhoneNumber ->
  PhoneType ->
  PhoneNumberContact ->
  m (Either UseCaseError ())
updatePhoneNumberAndPublishEvent input newPhoneNumber newPhoneType phoneNumberContact = do
  let updatedPhoneNumberContact = changePhoneNumber phoneNumberContact newPhoneNumber newPhoneType
  saveResult <- save updatedPhoneNumberContact
  case saveResult of
    Left err -> return $ Left $ createValidationError $ "Failed to save PhoneNumber: " <> pack (show err)
    Right _ -> publishPhoneNumberEvent input updatedPhoneNumberContact

publishPhoneNumberEvent ::
  (DomainEventPublisher m, Monad m) =>
  Input ->
  PhoneNumberContact ->
  m (Either UseCaseError ())
publishPhoneNumberEvent input updatedPhoneNumberContact = do
  let event = phoneNumberUpserted updatedPhoneNumberContact (updatedAt input)
  result <-
    publishEvent
      (accountId input)
      "account"
      "PhoneNumberContactUpserted"
      "system"
      event
      Nothing
  return $ case result of
    Left err -> Left $ mapDomainEventErrorToUseCaseError err
    Right _ -> Right ()
