module Application.BankAccount.UseCases.ApproveAccountUseCase
  ( Input (..),
    execute,
  )
where

import Application.UseCaseError
  ( UseCaseError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.ApproveAccountFactory (createApproveAccount)
import Domain.BankAccount.Entity.ApproveAccount
  ( accountApproved,
  )
import qualified Domain.BankAccount.Events.AccountApproved as AccountApproved
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

data Input = Input
  { accountId :: UUID,
    approvedAt :: UTCTime,
    approvalNotes :: Maybe Text
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
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
        Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
        Right _ -> return $ Right ()
