{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.BuildingNameSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.BuildingName
import Domain.ValueError (unwrapValueError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkBuildingName" $ do
    it "should create a BuildingName for valid input" $ do
      case mkBuildingName "Valid Building Name" of
        Right buildingName -> unwrapBuildingName buildingName `shouldBe` "Valid Building Name"
        Left _ -> expectationFailure "Expected a valid BuildingName"

    it "should trim whitespace from input" $ do
      case mkBuildingName "  Valid Building Name  " of
        Right buildingName -> unwrapBuildingName buildingName `shouldBe` "Valid Building Name"
        Left _ -> expectationFailure "Expected a valid BuildingName"

    it "should return an error for empty input" $ do
      case mkBuildingName "" of
        Left err -> unwrapValueError err `shouldBe` "Building name cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid BuildingName"

    it "should return an error for whitespace-only input" $ do
      case mkBuildingName "    " of
        Left err -> unwrapValueError err `shouldBe` "Building name cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid BuildingName"

    it "should create a BuildingName for 255 characters" $ do
      let input = T.replicate 255 "a"
      case mkBuildingName input of
        Right buildingName -> unwrapBuildingName buildingName `shouldBe` input
        Left _ -> expectationFailure "Expected a valid BuildingName"

    it "should return an error for input longer than 255 characters" $ do
      let input = T.replicate 256 "a"
      case mkBuildingName input of
        Left err -> unwrapValueError err `shouldBe` "Building name cannot exceed 255 characters (got 256)."
        Right _ -> expectationFailure "Expected an invalid BuildingName"
