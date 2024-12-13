module Application.BankAccount.UseCases.UpsertUserContactInfoUseCase where

import Application.UseCaseError
  ( UseCaseError,
    createSystemError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.UserContactInfoUpserted as UserContactInfoUpserted
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

data Input = Input
  { accountId :: UUID,
    email :: Text,
    updatedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let event =
        UserContactInfoUpserted.UserContactInfoUpserted
          { UserContactInfoUpserted.accountId = accountId input,
            UserContactInfoUpserted.email = email input,
            UserContactInfoUpserted.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "UserContactInfoUpserted" "system" event Nothing

  case result of
    Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
    Right _ -> return $ Right ()
