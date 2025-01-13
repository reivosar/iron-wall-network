{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.ActivateAccountUseCase
  ( Input (..),
    execute,
  )
where

import Application.BankAccount.Factories.ActiveAccountFactory
import Application.UseCaseError
  ( UseCaseError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.ActiveAccount
  ( accountActivated,
  )
import qualified Domain.BankAccount.Events.AccountActivated as AccountActivated
import Domain.DomainEventPublisher
import Domain.Error (unwrapDomainError)

data Input = Input
  { accountId :: UUID,
    password :: Text,
    activatedAt :: UTCTime
  }

execute :: (ActiveAccountFactory m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  createActiveAccountResult <-
    createActiveAccount
      (accountId input)
      (password input)
      (activatedAt input)

  case createActiveAccountResult of
    Left err -> return $ Left (createValidationError (unwrapDomainError err))
    Right activeAccount -> do
      let event = accountActivated activeAccount

      result <-
        publishEvent
          (AccountActivated.accountId event)
          (aggregateTypeToText Account)
          AccountActivated.eventName
          "system"
          event
          Nothing

      case result of
        Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
        Right _ -> return $ Right ()
