{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertEmergencyContactUseCase
  ( Input (..),
    execute,
  )
where

import Application.UseCaseError
  ( UseCaseError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class
  ( MonadIO,
  )
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as EmergencyContactUpserted
import Domain.DomainEventPublisher

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
