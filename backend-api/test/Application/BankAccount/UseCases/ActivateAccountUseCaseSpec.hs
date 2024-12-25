{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.ActivateAccountUseCaseSpec (spec) where

import Application.BankAccount.Factories.ActiveAccountFactory
import Application.BankAccount.UseCases.ActivateAccountUseCase
import Application.UseCaseError (UseCaseError, unwrapUseCaseError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, object, toJSON)
import Data.Aeson.Types (Object)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.ActiveAccount (ActiveAccount, accountActivated, mkActiveAccount)
import qualified Domain.BankAccount.Events.AccountActivated as AccountActivated
import Domain.BankAccount.Repositories.AccountRepository
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.BankAccount.ValueObject.AccountPassword (mkAccountPassword)
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError, mkValueError)
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockCreateActiveAccount :: UUID.UUID -> Text -> UTCTime -> IO (Either ValueError ActiveAccount),
    mockPublishEvent :: UUID.UUID -> Text -> Text -> Text -> AccountActivated.AccountActivated -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of ActiveAccountFactory
instance ActiveAccountFactory (ReaderT MockEnv IO) where
  createActiveAccount uuid password activatedAt = do
    env <- ask
    liftIO $ mockCreateActiveAccount env uuid password activatedAt

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe AccountActivated.AccountActivated
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully activate an account" $ do
      -- GIVEN
      let uuid = UUID.nil
      let password = "valid-password"
      currentTime <- getCurrentTime
      let accountPassword = either (error "Failed to create AccountPassword") id (mkAccountPassword password "test-secret-key")
      let activeAccount = mkActiveAccount (mkAccountId uuid) accountPassword currentTime
      let event = accountActivated activeAccount
      let mockEnv =
            MockEnv
              { mockCreateActiveAccount = \_ _ _ -> return $ Right activeAccount,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, password = password, activatedAt = currentTime}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if ActiveAccount creation fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      let password = "invalid-password"
      currentTime <- getCurrentTime
      let mockEnv =
            MockEnv
              { mockCreateActiveAccount = \_ _ _ -> return $ Left (mkValueError "Invalid password"),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, password = password, activatedAt = currentTime}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid password"
        Right _ -> expectationFailure "Expected an error but got a success"

    it "should return an error if event publishing fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      let password = "valid-password"
      currentTime <- getCurrentTime
      let accountPassword = either (error "Failed to create AccountPassword") id (mkAccountPassword password "test-secret-key")
      let activeAccount = mkActiveAccount (mkAccountId uuid) accountPassword currentTime
      let event = accountActivated activeAccount
      let mockEnv =
            MockEnv
              { mockCreateActiveAccount = \_ _ _ -> return $ Right activeAccount,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Left (PublishEventFailed "Event publishing failed")
              }
      let input = Input {accountId = uuid, password = password, activatedAt = currentTime}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Event publishing failed: Event publishing failed"
        Right _ -> expectationFailure "Expected an error but got a success"
