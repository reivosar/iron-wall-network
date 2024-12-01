module Application.BankAccount.UseCases.SuspendAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.AccountSuspended as AccountSuspended
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    reason :: Maybe Text,
    suspendedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        AccountSuspended.AccountSuspended
          { AccountSuspended.accountId = accountId input,
            AccountSuspended.reason = reason input,
            AccountSuspended.suspendedAt = suspendedAt input
          }

  result <- publishEvent (accountId input) "account" "AccountSuspended" "system" event Nothing

  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
