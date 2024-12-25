{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.ApproveAccountUseCaseSpec (spec) where

import Application.BankAccount.Factories.ApproveAccountFactory
import Application.BankAccount.UseCases.ApproveAccountUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.ApproveAccount (ApproveAccount, mkApproveAccount)
import qualified Domain.BankAccount.Events.AccountApproved as AccountApproved
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError, mkValueError)
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockCreateApproveAccount :: UUID.UUID -> UTCTime -> Maybe Text -> IO (Either ValueError ApproveAccount),
    mockPublishEvent :: UUID.UUID -> Text -> Text -> Text -> AccountApproved.AccountApproved -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of ApproveAccountFactory
instance ApproveAccountFactory (ReaderT MockEnv IO) where
  createApproveAccount uuid currentApprovedAt apprvlNts = do
    env <- ask
    liftIO $ mockCreateApproveAccount env uuid currentApprovedAt apprvlNts

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe AccountApproved.AccountApproved
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully approve an account" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentApprovedAt <- liftIO getCurrentTime
      let apprvlNts = Just "Approved by system"
      let acctId = mkAccountId uuid
      let approveAccount = mkApproveAccount acctId currentApprovedAt apprvlNts
      let mockEnv =
            MockEnv
              { mockCreateApproveAccount = \_ _ _ -> return $ Right approveAccount,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, approvedAt = currentApprovedAt, approvalNotes = apprvlNts}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if ApproveAccount creation fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentApprovedAt <- liftIO getCurrentTime
      let apprvlNts = Just "Invalid approval"
      let mockEnv =
            MockEnv
              { mockCreateApproveAccount = \_ _ _ -> return $ Left (mkValueError "Invalid approval details"),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, approvedAt = currentApprovedAt, approvalNotes = apprvlNts}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid approval details"
        Right _ -> expectationFailure "Expected an error but got a success"

    it "should return an error if event publishing fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentApprovedAt <- liftIO getCurrentTime
      let apprvlNts = Just "Approved by system"
      let acctId = mkAccountId uuid
      let approveAccount = mkApproveAccount acctId currentApprovedAt apprvlNts
      let mockEnv =
            MockEnv
              { mockCreateApproveAccount = \_ _ _ -> return $ Right approveAccount,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Left (PublishEventFailed "Event publishing failed")
              }
      let input = Input {accountId = uuid, approvedAt = currentApprovedAt, approvalNotes = apprvlNts}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Event publishing failed: Event publishing failed"
        Right _ -> expectationFailure "Expected an error but got a success"
