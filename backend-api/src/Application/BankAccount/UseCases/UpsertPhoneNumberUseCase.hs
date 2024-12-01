module Application.BankAccount.UseCases.UpsertPhoneNumberUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.PhoneNumberUpserted as PhoneNumberUpserted
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    phoneNumber :: Text,
    phoneType :: Text,
    updatedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        PhoneNumberUpserted.PhoneNumberUpserted
          { PhoneNumberUpserted.accountId = accountId input,
            PhoneNumberUpserted.phoneNumber = phoneNumber input,
            PhoneNumberUpserted.phoneType = phoneType input,
            PhoneNumberUpserted.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "PhoneNumberUpserted" "system" event Nothing

  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
