module Application.BankAccount.UseCases.SuspendAccountUseCase
  ( Input (..),
    execute,
  )
where

import Application.BankAccount.Factories.SuspendAccountFactory (createSuspendAccount)
import Application.UseCaseError
  ( UseCaseError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.SuspendAccount
  ( accountSuspended,
  )
import qualified Domain.BankAccount.Events.AccountSuspended as AccountSuspended
import Domain.DomainEventPublisher
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    reason :: Maybe Text,
    suspendedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  case createSuspendAccount (accountId input) (suspendedAt input) (reason input) of
    Left err -> return $ Left (createValidationError (unwrapValueError err))
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
