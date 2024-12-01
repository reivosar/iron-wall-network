{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.UseCases.UpsertEmergencyContactUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as EmergencyContactUpserted
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    contactName :: Text,
    contactPhone :: Text,
    updatedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        EmergencyContactUpserted.EmergencyContactUpserted
          { EmergencyContactUpserted.accountId = accountId input,
            EmergencyContactUpserted.contactName = contactName input,
            EmergencyContactUpserted.contactPhone = contactPhone input,
            EmergencyContactUpserted.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "EmergencyContactUpserted" "system" event Nothing

  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
