{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertPhoneNumberUseCase
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
import qualified Domain.BankAccount.Events.PhoneNumberUpserted as PhoneNumberUpserted
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
        PhoneNumberUpserted.PhoneNumberUpserted
          { PhoneNumberUpserted.accountId = accountId input,
            PhoneNumberUpserted.phoneNumber = phoneNumber input,
            PhoneNumberUpserted.phoneType = phoneType input,
            PhoneNumberUpserted.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "PhoneNumberUpserted" "system" event Nothing

  case result of
    Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
    Right _ -> return $ Right ()
