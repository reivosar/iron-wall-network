module Application.BankAccount.UseCases.SuspendAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.SuspendAccount (SuspendAccount, accountSuspended)
import qualified Domain.BankAccount.Events.AccountSuspended as AccountSuspended
import Domain.BankAccount.SuspendAccountFactory (createSuspendAccount)
import Domain.ValueError (ValueError (..))
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    reason :: Maybe Text,
    suspendedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  case createSuspendAccount (accountId input) (suspendedAt input) (reason input) of
    Left (ValueError msg) -> return $ Left (createValidationError msg)
    Right suspendAccount -> do
      let event = accountSuspended suspendAccount

      result <-
        publishEvent
          (AccountSuspended.accountId event)
          "account"
          "AccountSuspended"
          "system"
          event
          Nothing

      case result of
        Left (RedisConnectionError msg) ->
          return $ Left (createSystemError ("Redis connection error: " ++ show msg))
        Left (RedisCommandError msg) ->
          return $ Left (createSystemError ("Redis command error: " ++ show msg))
        Left (EventStoreError msg) ->
          return $ Left (createValidationError ("Failed to store event: " ++ show msg))
        Right _ -> return $ Right ()
