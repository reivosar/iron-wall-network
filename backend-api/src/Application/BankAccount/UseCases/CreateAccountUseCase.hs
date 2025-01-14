{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.CreateAccountUseCase
  ( Input (..),
    execute,
  )
where

import Application.BankAccount.Factories.BankAccountFactory
import Application.UseCaseError
  ( UseCaseError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.InitialAccount
  ( accountCreated,
  )
import qualified Domain.BankAccount.Events.AccountCreated as AccountCreated
import Domain.DomainEventPublisher
import Domain.Error (unwrapDomainError)
import Infrastructure.Repositories.BankAccount.UUIDAccountRepository

data Input = Input
  { username :: Text,
    fullName :: Text,
    email :: Text,
    createdAt :: UTCTime
  }

execute :: (BankAccountFactory m, DomainEventPublisher m) => Input -> m (Either UseCaseError UUID)
execute input = do
  createBankAccountResult <-
    createBankAccount
      (username input)
      (fullName input)
      (email input)
      (createdAt input)

  case createBankAccountResult of
    Left err -> return $ Left (createValidationError (unwrapDomainError err))
    Right bankAccount -> do
      let event = accountCreated bankAccount

      result <-
        publishEvent
          (AccountCreated.accountId event)
          (aggregateTypeToText Account)
          AccountCreated.eventName
          "system"
          event
          Nothing

      case result of
        Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
        Right _ -> return $ Right (AccountCreated.accountId event)
