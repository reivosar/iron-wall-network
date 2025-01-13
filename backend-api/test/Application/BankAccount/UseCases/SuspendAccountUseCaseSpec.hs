{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.SuspendAccountUseCaseSpec (spec) where

import Application.BankAccount.Factories.SuspendAccountFactory
import Application.BankAccount.UseCases.SuspendAccountUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.SuspendAccount (SuspendAccount, mkSuspendAccount)
import qualified Domain.BankAccount.Events.AccountSuspended as AccountSuspended
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.DomainEventPublisher
import Domain.Error (DomainError, mkDomainError)
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockCreateSuspendAccount :: UUID.UUID -> UTCTime -> Maybe Text -> IO (Either DomainError SuspendAccount),
    mockPublishEvent :: UUID.UUID -> Text -> Text -> Text -> AccountSuspended.AccountSuspended -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of SuspendAccountFactory
instance SuspendAccountFactory (ReaderT MockEnv IO) where
  createSuspendAccount uuid suspendedAtParam reasonParam = do
    env <- ask
    liftIO $ mockCreateSuspendAccount env uuid suspendedAtParam reasonParam

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe AccountSuspended.AccountSuspended
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully suspend an account" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentSuspendedAt <- liftIO getCurrentTime
      let suspensionReason = Just "Suspended due to suspicious activity"
      let acctId = mkAccountId uuid
      let suspendAccount = mkSuspendAccount acctId currentSuspendedAt suspensionReason
      let mockEnv =
            MockEnv
              { mockCreateSuspendAccount = \_ _ _ -> return $ Right suspendAccount,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, suspendedAt = currentSuspendedAt, reason = suspensionReason}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if SuspendAccount creation fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentSuspendedAt <- liftIO getCurrentTime
      let invalidReason = Just "Invalid suspension reason"
      let mockEnv =
            MockEnv
              { mockCreateSuspendAccount = \_ _ _ -> return $ Left (mkDomainError "Invalid suspension details"),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, suspendedAt = currentSuspendedAt, reason = invalidReason}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid suspension details"
        Right _ -> expectationFailure "Expected an error but got a success"

    it "should return an error if event publishing fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentSuspendedAt <- liftIO getCurrentTime
      let suspensionReason = Just "Suspended due to suspicious activity"
      let acctId = mkAccountId uuid
      let suspendAccount = mkSuspendAccount acctId currentSuspendedAt suspensionReason
      let mockEnv =
            MockEnv
              { mockCreateSuspendAccount = \_ _ _ -> return $ Right suspendAccount,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Left (PublishEventFailed "Event publishing failed")
              }
      let input = Input {accountId = uuid, suspendedAt = currentSuspendedAt, reason = suspensionReason}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Event publishing failed: Event publishing failed"
        Right _ -> expectationFailure "Expected an error but got a success"
