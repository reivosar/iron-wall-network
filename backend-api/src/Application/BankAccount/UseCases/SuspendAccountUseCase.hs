module Application.BankAccount.UseCases.SuspendAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError, mapDomainEventErrorToUseCaseError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.SuspendAccount (SuspendAccount, accountSuspended)
import qualified Domain.BankAccount.Events.AccountSuspended as AccountSuspended
import Domain.BankAccount.SuspendAccountFactory (createSuspendAccount)
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

data Input = Input
  { accountId :: UUID,
    reason :: Maybe Text,
    suspendedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  case createSuspendAccount (accountId input) (suspendedAt input) (reason input) of
    Left (ValueError msg) -> return $ Left (createValidationError msg)
    Right suspendAccount -> do
      let event = accountSuspended suspendAccount

      result <-
        publishEvent
          (AccountSuspended.accountId event)
          "account"
          "AccountSuspended"
          "system"
          event
          Nothing

      case result of
        Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
        Right _ -> return $ Right ()
