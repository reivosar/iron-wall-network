module Application.BankAccount.UseCases.ApproveAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.AccountApproved as AccountApproved
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    approvedAt :: UTCTime,
    approvalNotes :: Maybe Text
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        AccountApproved.AccountApproved
          { AccountApproved.accountId = accountId input,
            AccountApproved.approvedAt = approvedAt input,
            AccountApproved.approvalNotes = approvalNotes input
          }
  result <- publishEvent (accountId input) "account" "AccountApproved" "system" event Nothing
  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
