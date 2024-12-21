module Application.BankAccount.UseCases.CloseAccountUseCase
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
import Domain.BankAccount.CloseAccountFactory (createCloseAccount)
import Domain.BankAccount.Entity.CloseAccount
  ( accountClosed,
  )
import qualified Domain.BankAccount.Events.AccountClosed as AccountClosed
import Domain.DomainEventPublisher
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    closedAt :: UTCTime,
    reason :: Maybe Text
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  case createCloseAccount (accountId input) (closedAt input) (reason input) of
    Left err -> return $ Left (createValidationError (unwrapValueError err))
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
