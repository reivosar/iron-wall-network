{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.CloseAccountUseCaseSpec (spec) where

import Application.BankAccount.Factories.CloseAccountFactory
import Application.BankAccount.UseCases.CloseAccountUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.CloseAccount (CloseAccount, mkCloseAccount)
import qualified Domain.BankAccount.Events.AccountClosed as AccountClosed
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.DomainEventPublisher
import Domain.Error (DomainError, mkDomainError)
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockCreateCloseAccount :: UUID.UUID -> UTCTime -> Maybe Text -> IO (Either DomainError CloseAccount),
    mockPublishEvent :: UUID.UUID -> Text -> Text -> Text -> AccountClosed.AccountClosed -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of CloseAccountFactory
instance CloseAccountFactory (ReaderT MockEnv IO) where
  createCloseAccount uuid closeTime reasonDetails = do
    env <- ask
    liftIO $ mockCreateCloseAccount env uuid closeTime reasonDetails

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe AccountClosed.AccountClosed
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully close an account" $ do
      -- GIVEN
      let uuid = UUID.nil
      closeTime <- liftIO getCurrentTime
      let reasonDetails = Just "Closed by user request"
      let accountIdObj = mkAccountId uuid
      let closeAccountEntity = mkCloseAccount accountIdObj closeTime reasonDetails
      let mockEnv =
            MockEnv
              { mockCreateCloseAccount = \_ _ _ -> return $ Right closeAccountEntity,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let inputData = Input {accountId = uuid, closedAt = closeTime, reason = reasonDetails}

      -- WHEN
      result <- runReaderT (execute inputData) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if CloseAccount creation fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      closeTime <- liftIO getCurrentTime
      let reasonDetails = Just "Invalid reason"
      let mockEnv =
            MockEnv
              { mockCreateCloseAccount = \_ _ _ -> return $ Left (mkDomainError "Invalid closure details"),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let inputData = Input {accountId = uuid, closedAt = closeTime, reason = reasonDetails}

      -- WHEN
      result <- runReaderT (execute inputData) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid closure details"
        Right _ -> expectationFailure "Expected an error but got a success"

    it "should return an error if event publishing fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      closeTime <- liftIO getCurrentTime
      let reasonDetails = Just "Closed by user request"
      let accountIdObj = mkAccountId uuid
      let closeAccountEntity = mkCloseAccount accountIdObj closeTime reasonDetails
      let mockEnv =
            MockEnv
              { mockCreateCloseAccount = \_ _ _ -> return $ Right closeAccountEntity,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Left (PublishEventFailed "Event publishing failed")
              }
      let inputData = Input {accountId = uuid, closedAt = closeTime, reason = reasonDetails}

      -- WHEN
      result <- runReaderT (execute inputData) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Event publishing failed: Event publishing failed"
        Right _ -> expectationFailure "Expected an error but got a success"
