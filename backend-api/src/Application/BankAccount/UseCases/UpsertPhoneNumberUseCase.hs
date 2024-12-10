module Application.BankAccount.UseCases.UpsertPhoneNumberUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError, mapDomainEventErrorToUseCaseError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.PhoneNumberUpserted as PhoneNumberUpserted
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

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
