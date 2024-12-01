module Application.BankAccount.UseCases.CloseAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.AccountClosed as AccountClosed
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    closedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        AccountClosed.AccountClosed
          { AccountClosed.accountId = accountId input,
            AccountClosed.closedAt = closedAt input
          }

  result <- publishEvent (accountId input) "account" "AccountClosed" "system" event Nothing

  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
