{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertPhoneNumberContactUseCase
  ( Input (..),
    execute,
  )
where

import Application.UseCaseError
  ( UseCaseError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.PhoneNumberContactUpserted as Event
import Domain.DomainEventPublisher

data Input = Input
  { accountId :: UUID,
    phoneNumber :: Text,
    phoneType :: Text,
    updatedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let event =
        Event.PhoneNumberContactUpserted
          { Event.accountId = accountId input,
            Event.phoneNumber = phoneNumber input,
            Event.phoneType = phoneType input,
            Event.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "PhoneNumberContactUpserted" "system" event Nothing

  case result of
    Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
    Right _ -> return $ Right ()
