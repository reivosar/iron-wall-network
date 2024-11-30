{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.UseCases.CreateAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.BankAccountFactory (createBankAccount)
import Domain.BankAccount.Entity.BankAccount (BankAccount, accountCreated)
import qualified Domain.BankAccount.Events.AccountCreated as AccountCreated
import Domain.ValueError (ValueError (..))
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { username :: Text,
    fullName :: Text,
    email :: Text,
    createdAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError UUID)
execute input = do
  createBankAccountResult <-
    createBankAccount
      (username input)
      (fullName input)
      (email input)
      (createdAt input)

  case createBankAccountResult of
    Left (ValueError msg) -> return $ Left (createValidationError msg)
    Right bankAccount -> do
      let event = accountCreated bankAccount

      result <-
        publishEvent
          (AccountCreated.accountId event)
          "account"
          "AccountCreated"
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
        Right _ -> return $ Right (AccountCreated.accountId event)
