{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.UseCases.UpsertEmergencyContactUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError, mapDomainEventErrorToUseCaseError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as EmergencyContactUpserted
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

data Input = Input
  { accountId :: UUID,
    contactName :: Text,
    contactPhone :: Text,
    updatedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let event =
        EmergencyContactUpserted.EmergencyContactUpserted
          { EmergencyContactUpserted.accountId = accountId input,
            EmergencyContactUpserted.contactName = contactName input,
            EmergencyContactUpserted.contactPhone = contactPhone input,
            EmergencyContactUpserted.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "EmergencyContactUpserted" "system" event Nothing

  case result of
    Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
    Right _ -> return $ Right ()
