{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.UpsertEmergencyContactUseCaseSpec (spec) where

import Application.BankAccount.UseCases.UpsertEmergencyContactUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Exception (SomeException, toException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID, nil)
import Domain.BankAccount.Entity.EmergencyContact (EmergencyContact, mkEmergencyContact)
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as EmergencyContactUpserted
import Domain.BankAccount.Repositories.EmergencyContactRepository
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.PhoneNumber (mkPhoneNumber)
import Domain.DomainEventPublisher
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockFindById :: AccountId -> IO (Either SomeException (Maybe EmergencyContact)),
    mockSave :: EmergencyContact -> IO (Either SomeException ()),
    mockPublishEvent :: UUID -> Text -> Text -> Text -> EmergencyContactUpserted.EmergencyContactUpserted -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of EmergencyContactRepository
instance EmergencyContactRepository (ReaderT MockEnv IO) where
  findById accId = do
    env <- ask
    liftIO $ mockFindById env accId
  save emergencyContact = do
    env <- ask
    liftIO $ mockSave env emergencyContact

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe EmergencyContactUpserted.EmergencyContactUpserted
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully upsert an emergency contact" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                contactName = "John Doe",
                contactPhone = "090-1234-5678",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if FullName is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                contactName = "",
                contactPhone = "090-1234-5678",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid FullName: Full name cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if PhoneNumber is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                contactName = "John Doe",
                contactPhone = "invalid-phone",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid PhoneNumber: Invalid phone number format."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should successfully update an existing emergency contact" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let existingEmergencyContact =
            mkEmergencyContact
              (mkAccountId nil)
              (fromRight (error "Invalid FullName") $ mkFullName "Old Name")
              (fromRight (error "Invalid PhoneNumber") $ mkPhoneNumber "080-8765-4321")
      let input =
            Input
              { accountId = nil,
                contactName = "John Doe",
                contactPhone = "090-1234-5678",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right (Just existingEmergencyContact),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if findById fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                contactName = "John Doe",
                contactPhone = "090-1234-5678",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Left (toException $ userError "Database error"),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to fetch EmergencyContact: user error (Database error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if save fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                contactName = "John Doe",
                contactPhone = "090-1234-5678",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Left (toException $ userError "Save error"),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to save EmergencyContact: user error (Save error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if publishEvent fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                contactName = "John Doe",
                contactPhone = "090-1234-5678",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Left (PublishEventFailed "Publish error")
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Event publishing failed: Publish error"
        Right _ -> expectationFailure "Expected an error but got success"
