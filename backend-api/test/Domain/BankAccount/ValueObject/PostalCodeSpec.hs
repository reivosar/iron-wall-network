{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.PostalCodeSpec (spec) where

import Domain.BankAccount.ValueObject.PostalCode
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkPostalCode" $ do
    it "should create a PostalCode for valid input" $ do
      case mkPostalCode "123-4567" of
        Right postalCode -> unwrapPostalCode postalCode `shouldBe` "123-4567"
        Left _ -> expectationFailure "Expected a valid PostalCode"

    it "should trim whitespace from input" $ do
      case mkPostalCode "  123-4567  " of
        Right postalCode -> unwrapPostalCode postalCode `shouldBe` "123-4567"
        Left _ -> expectationFailure "Expected a valid PostalCode"

    it "should return an error for empty input" $ do
      case mkPostalCode "" of
        Left err -> unwrapDomainError err `shouldBe` "Postal code cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid PostalCode"

    it "should return an error for whitespace-only input" $ do
      case mkPostalCode "    " of
        Left err -> unwrapDomainError err `shouldBe` "Postal code cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid PostalCode"

    it "should return an error for an invalid format" $ do
      case mkPostalCode "1234567" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid postal code format. Expected format: 123-4567."
        Right _ -> expectationFailure "Expected an invalid PostalCode"

    it "should return an error for an incomplete postal code" $ do
      case mkPostalCode "123-" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid postal code format. Expected format: 123-4567."
        Right _ -> expectationFailure "Expected an invalid PostalCode"

    it "should return an error for input longer than the expected format" $ do
      case mkPostalCode "123-45678" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid postal code format. Expected format: 123-4567."
        Right _ -> expectationFailure "Expected an invalid PostalCode"
