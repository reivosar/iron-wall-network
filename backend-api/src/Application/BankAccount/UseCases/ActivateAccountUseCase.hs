module Application.BankAccount.UseCases.ActivateAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.AccountActivated as AccountActivated
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    activatedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        AccountActivated.AccountActivated
          { AccountActivated.accountId = accountId input,
            AccountActivated.activatedAt = activatedAt input
          }
  result <- publishEvent (accountId input) "account" "AccountActivated" "system" event Nothing
  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
