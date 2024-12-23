{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertAddressUseCase
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
import qualified Domain.BankAccount.Events.AddressUpserted as AddressUpserted
import Domain.DomainEventPublisher

data Input = Input
  { accountId :: UUID,
    address :: Text,
    addressType :: Text,
    updatedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let event =
        AddressUpserted.AddressUpserted
          { AddressUpserted.accountId = accountId input,
            AddressUpserted.address = address input,
            AddressUpserted.addressType = addressType input,
            AddressUpserted.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "AddressUpserted" "system" event Nothing

  case result of
    Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
    Right _ -> return $ Right ()
