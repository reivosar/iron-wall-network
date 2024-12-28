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
import qualified Domain.BankAccount.Events.AddressUpserted as Event
import Domain.DomainEventPublisher

data Input = Input
  { accountId :: UUID,
    postalCode :: Text,
    prefecture :: Text,
    city :: Text,
    townArea :: Text,
    buildingName :: Maybe Text,
    addressType :: Text,
    updatedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let event =
        Event.AddressUpserted
          { Event.accountId = accountId input,
            Event.postalCode = postalCode input,
            Event.prefecture = prefecture input,
            Event.city = city input,
            Event.townArea = townArea input,
            Event.buildingName = buildingName input,
            Event.addressType = addressType input,
            Event.updatedAt = updatedAt input
          }
  result <- publishEvent (accountId input) "account" "AddressUpserted" "system" event Nothing
  case result of
    Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
    Right _ -> return $ Right ()
