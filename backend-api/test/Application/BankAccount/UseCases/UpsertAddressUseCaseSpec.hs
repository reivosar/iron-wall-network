{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.UpsertAddressUseCaseSpec (spec) where

import Application.BankAccount.UseCases.UpsertAddressUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Exception (SomeException, toException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID, nil)
import Domain.BankAccount.Entity.Address (Address, mkAddress)
import qualified Domain.BankAccount.Events.AddressUpserted as AddressUpserted
import Domain.BankAccount.Repositories.AddressRepository
import Domain.BankAccount.Repositories.AddressRepository (AddressRepository, findById, save)
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.AddressType (textToAddressType)
import Domain.BankAccount.ValueObject.BuildingName (mkBuildingName)
import Domain.BankAccount.ValueObject.City (mkCity)
import Domain.BankAccount.ValueObject.PostalCode (mkPostalCode)
import Domain.BankAccount.ValueObject.Prefecture (mkPrefecture)
import Domain.BankAccount.ValueObject.TownArea (mkTownArea)
import Domain.DomainEventPublisher
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockFindById :: AccountId -> IO (Either SomeException (Maybe Address)),
    mockSave :: Address -> IO (Either SomeException ()),
    mockPublishEvent :: UUID -> Text -> Text -> Text -> AddressUpserted.AddressUpserted -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of AddressRepository
instance AddressRepository (ReaderT MockEnv IO) where
  findById accId = do
    env <- ask
    liftIO $ mockFindById env accId
  save address = do
    env <- ask
    liftIO $ mockSave env address

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe AddressUpserted.AddressUpserted
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully upsert an address" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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

    it "should return an error if PostalCode is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid PostalCode: Postal code cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if Prefecture is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid Prefecture: Prefecture cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if City is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid City: City cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if AddressType is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "invalid",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid AddressType: Invalid AddressType. Expected 'home', or 'office'."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should successfully upsert an address when no existing address is found" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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

    it "should successfully update an existing address" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let postalCodeVo = fromRight (error "Invalid PostalCode") $ mkPostalCode "160-0023"
      let prefectureVo = fromRight (error "Invalid Prefecture") $ mkPrefecture "東京都"
      let cityVo = fromRight (error "Invalid City") $ mkCity "新宿区"
      let townAreaVo = fromRight (error "Invalid TownArea") $ mkTownArea "西新宿"
      let buildingNameVo = Just $ fromRight (error "Invalid BuildingName") $ mkBuildingName "旧NSビル"
      let addressTypeVo = fromRight (error "Invalid AddressType") $ textToAddressType "home"
      let existingAddress = mkAddress (mkAccountId nil) postalCodeVo prefectureVo cityVo townAreaVo buildingNameVo addressTypeVo
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "新NSビル",
                addressType = "home",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right (Just existingAddress),
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
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to fetch Address: user error (Database error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if save fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to save Address: user error (Save error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if publishEvent fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                postalCode = "160-0023",
                prefecture = "東京都",
                city = "新宿区",
                townArea = "西新宿",
                buildingName = Just "NSビル",
                addressType = "home",
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
