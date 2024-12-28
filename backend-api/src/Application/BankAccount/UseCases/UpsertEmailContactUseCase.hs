{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.UpsertEmailContactUseCase
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
import qualified Domain.BankAccount.Events.EmailContactUpserted as Event
import Domain.DomainEventPublisher

data Input = Input
  { accountId :: UUID,
    email :: Text,
    updatedAt :: UTCTime
  }

execute :: (DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let event =
        Event.EmailContactUpserted
          { Event.accountId = accountId input,
            Event.email = email input,
            Event.updatedAt = updatedAt input
          }

  result <- publishEvent (accountId input) "account" "EmailContactUpserted" "system" event Nothing

  case result of
    Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
    Right _ -> return $ Right ()
