module Application.BankAccount.UseCases.UpsertUserContactInfoUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.UserContactInfoUpserted as UserContactInfoUpserted
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    email :: Text,
    updatedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        UserContactInfoUpserted.UserContactInfoUpserted
          { UserContactInfoUpserted.accountId = accountId input,
            UserContactInfoUpserted.email = email input,
            UserContactInfoUpserted.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "UserContactInfoUpserted" "system" event Nothing

  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
