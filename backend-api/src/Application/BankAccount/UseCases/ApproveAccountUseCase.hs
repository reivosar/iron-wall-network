module Application.BankAccount.UseCases.ApproveAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.ApproveAccountFactory (createApproveAccount)
import Domain.BankAccount.Entity.ApproveAccount (ApproveAccount, accountApproved)
import qualified Domain.BankAccount.Events.AccountApproved as AccountApproved
import Domain.ValueError (ValueError (..))
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    approvedAt :: UTCTime,
    approvalNotes :: Maybe Text
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  case createApproveAccount (accountId input) (approvedAt input) (approvalNotes input) of
    Left (ValueError msg) -> return $ Left (createValidationError msg)
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
        Left (RedisConnectionError msg) ->
          return $ Left (createSystemError ("Redis connection error: " ++ show msg))
        Left (RedisCommandError msg) ->
          return $ Left (createSystemError ("Redis command error: " ++ show msg))
        Left (EventStoreError msg) ->
          return $ Left (createValidationError ("Failed to store event: " ++ show msg))
        Right _ -> return $ Right ()
