{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.ApproveAccountUseCase
  ( Input (..),
    execute,
  )
where

import Application.BankAccount.Factories.ApproveAccountFactory
import Application.UseCaseError
  ( UseCaseError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.ApproveAccount
  ( accountApproved,
  )
import qualified Domain.BankAccount.Events.AccountApproved as AccountApproved
import Domain.DomainEventPublisher
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    approvedAt :: UTCTime,
    approvalNotes :: Maybe Text
  }

execute :: (ApproveAccountFactory m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  createApproveAccountResult <- createApproveAccount (accountId input) (approvedAt input) (approvalNotes input)
  case createApproveAccountResult of
    Left err -> return $ Left (createValidationError (unwrapValueError err))
    Right approveAccount -> do
      let event = accountApproved approveAccount

      result <-
        publishEvent
          (AccountApproved.accountId event)
          "account"
          "AccountApproved"
          "system"
          event
          Nothing

      case result of
        Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
        Right _ -> return $ Right ()
