module Application.BankAccount.UseCases.UpsertAddressUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.AddressUpserted as AddressUpserted
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    address :: Text,
    addressType :: Text,
    updatedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        AddressUpserted.AddressUpserted
          { AddressUpserted.accountId = accountId input,
            AddressUpserted.address = address input,
            AddressUpserted.addressType = addressType input,
            AddressUpserted.updatedAt = updatedAt input
          }
  result <- publishEvent (accountId input) "account" "AddressUpserted" "system" event Nothing
  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
