{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertEmailContactUseCase
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
import Domain.BankAccount.Entity.EmailContact (EmailContact, changeEmailContact, emailContactUpserted, mkEmailContact)
import qualified Domain.BankAccount.Events.EmailContactUpserted as EmailContactUpserted
import Domain.BankAccount.Repositories.EmailContactRepository (EmailContactRepository, findById, save)
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.Email (Email, mkEmail)
import Domain.DomainEventPublisher (DomainEventPublisher, publishEvent)
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    email :: Text,
    updatedAt :: UTCTime
  }

execute :: (EmailContactRepository m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let accId = mkAccountId (accountId input)
  let emailResult = mkEmail (email input)

  case emailResult of
    Right emailVo -> findEmailContact accId >>= processEmailContact input emailVo
    Left err -> return $ Left $ createValidationError $ "Invalid Email: " <> unwrapValueError err

findEmailContact ::
  (EmailContactRepository m, Monad m) =>
  AccountId ->
  m (Either UseCaseError (Maybe EmailContact))
findEmailContact accId = do
  result <- findById accId
  return $ case result of
    Left err -> Left $ createValidationError $ "Failed to fetch EmailContact: " <> pack (show err)
    Right contact -> Right contact

processEmailContact ::
  (EmailContactRepository m, DomainEventPublisher m, MonadIO m) =>
  Input ->
  Email ->
  Either UseCaseError (Maybe EmailContact) ->
  m (Either UseCaseError ())
processEmailContact _ _ (Left err) = return $ Left err
processEmailContact input newEmail (Right Nothing) = do
  let newEmailContact = mkEmailContact (mkAccountId (accountId input)) newEmail
  updateEmailContactAndPublishEvent input newEmailContact
processEmailContact input newEmail (Right (Just existingEmailContact)) = do
  let updatedEmailContact = changeEmailContact existingEmailContact newEmail
  updateEmailContactAndPublishEvent input updatedEmailContact

updateEmailContactAndPublishEvent ::
  (EmailContactRepository m, DomainEventPublisher m, MonadIO m) =>
  Input ->
  EmailContact ->
  m (Either UseCaseError ())
updateEmailContactAndPublishEvent input emailContact = do
  saveResult <- save emailContact
  case saveResult of
    Left err -> return $ Left $ createValidationError $ "Failed to save EmailContact: " <> pack (show err)
    Right _ -> publishEmailContactEvent input emailContact

publishEmailContactEvent ::
  (DomainEventPublisher m, Monad m) =>
  Input ->
  EmailContact ->
  m (Either UseCaseError ())
publishEmailContactEvent input emailContact = do
  let event = emailContactUpserted emailContact (updatedAt input)

  result <-
    publishEvent
      (accountId input)
      (aggregateTypeToText Account)
      EmailContactUpserted.eventName
      "system"
      event
      Nothing

  return $ case result of
    Left err -> Left $ mapDomainEventErrorToUseCaseError err
    Right _ -> Right ()
