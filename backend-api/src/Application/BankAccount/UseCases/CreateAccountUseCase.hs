{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.UseCases.CreateAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError, mapDomainEventErrorToUseCaseError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.BankAccountFactory (createBankAccount)
import Domain.BankAccount.Entity.InitialAccount (InitialAccount, accountCreated)
import qualified Domain.BankAccount.Events.AccountCreated as AccountCreated
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

data Input = Input
  { username :: Text,
    fullName :: Text,
    email :: Text,
    createdAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError UUID)
execute input = do
  createBankAccountResult <-
    liftIO $
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
        Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
        Right _ -> return $ Right (AccountCreated.accountId event)
