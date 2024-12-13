module Application.BankAccount.UseCases.UpsertAddressUseCase
  ( Input (..),
    execute,
  )
where

import Application.UseCaseError
  ( UseCaseError,
    createSystemError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.AddressUpserted as AddressUpserted
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

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
