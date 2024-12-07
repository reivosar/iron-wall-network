module Application.BankAccount.UseCases.CloseAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.CloseAccountFactory (createCloseAccount)
import Domain.BankAccount.Entity.CloseAccount (CloseAccount, accountClosed)
import qualified Domain.BankAccount.Events.AccountClosed as AccountClosed
import Domain.ValueError (ValueError (..))
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    closedAt :: UTCTime,
    reason :: Maybe Text
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  case createCloseAccount (accountId input) (closedAt input) (reason input) of
    Left (ValueError msg) -> return $ Left (createValidationError msg)
    Right closeAccount -> do
      let event = accountClosed closeAccount

      result <-
        publishEvent
          (AccountClosed.accountId event)
          "account"
          "AccountClosed"
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
