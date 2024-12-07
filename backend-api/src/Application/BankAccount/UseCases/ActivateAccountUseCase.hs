module Application.BankAccount.UseCases.ActivateAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.ActiveAccountFactory (createActiveAccount)
import Domain.BankAccount.Entity.ActiveAccount (ActiveAccount, accountActivated)
import qualified Domain.BankAccount.Events.AccountActivated as AccountActivated
import Domain.ValueError (ValueError (..))
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    password :: Text,
    activatedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  createActiveAccountResult <-
    createActiveAccount
      (accountId input)
      (password input)
      (activatedAt input)

  case createActiveAccountResult of
    Left (ValueError msg) -> return $ Left (createValidationError msg)
    Right activeAccount -> do
      let event = accountActivated activeAccount

      result <-
        publishEvent
          (AccountActivated.accountId event)
          "account"
          "AccountActivated"
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
