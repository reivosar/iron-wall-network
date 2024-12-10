module Application.BankAccount.UseCases.CloseAccountUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError, mapDomainEventErrorToUseCaseError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.CloseAccountFactory (createCloseAccount)
import Domain.BankAccount.Entity.CloseAccount (CloseAccount, accountClosed)
import qualified Domain.BankAccount.Events.AccountClosed as AccountClosed
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

data Input = Input
  { accountId :: UUID,
    closedAt :: UTCTime,
    reason :: Maybe Text
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  case createCloseAccount (accountId input) (closedAt input) (reason input) of
    Left (ValueError msg) -> return $ Left (createValidationError msg)
    Right closeAccount -> do
      let event = accountClosed closeAccount

      result <-
        publishEvent
          (AccountClosed.accountId event)
          "account"
          "AccountClosed"
          "system"
          event
          Nothing

      case result of
        Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
        Right _ -> return $ Right ()
