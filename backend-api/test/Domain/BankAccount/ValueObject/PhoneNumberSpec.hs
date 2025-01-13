{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.PhoneNumberSpec (spec) where

import Domain.BankAccount.ValueObject.PhoneNumber
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkPhoneNumber" $ do
    it "should create a PhoneNumber for a valid fixed-line number with 2-digit area code" $ do
      case mkPhoneNumber "03-1234-5678" of
        Right actual -> unwrapPhoneNumber actual `shouldBe` "03-1234-5678"
        Left _ -> expectationFailure "Expected a valid PhoneNumber"

    it "should create a PhoneNumber for a valid fixed-line number with 3-digit area code" $ do
      case mkPhoneNumber "046-123-4567" of
        Right actual -> unwrapPhoneNumber actual `shouldBe` "046-123-4567"
        Left _ -> expectationFailure "Expected a valid PhoneNumber"

    it "should create a PhoneNumber for a valid fixed-line number with 4-digit area code" $ do
      case mkPhoneNumber "0467-88-1234" of
        Right actual -> unwrapPhoneNumber actual `shouldBe` "0467-88-1234"
        Left _ -> expectationFailure "Expected a valid PhoneNumber"

    it "should create a PhoneNumber for a valid mobile number starting with 090" $ do
      case mkPhoneNumber "090-1234-5678" of
        Right actual -> unwrapPhoneNumber actual `shouldBe` "090-1234-5678"
        Left _ -> expectationFailure "Expected a valid PhoneNumber"

    it "should create a PhoneNumber for a valid mobile number starting with 080" $ do
      case mkPhoneNumber "080-9876-5432" of
        Right actual -> unwrapPhoneNumber actual `shouldBe` "080-9876-5432"
        Left _ -> expectationFailure "Expected a valid PhoneNumber"

    it "should create a PhoneNumber for a valid mobile number starting with 070" $ do
      case mkPhoneNumber "070-5678-1234" of
        Right actual -> unwrapPhoneNumber actual `shouldBe` "070-5678-1234"
        Left _ -> expectationFailure "Expected a valid PhoneNumber"

    it "should return an error for an invalid area code" $ do
      case mkPhoneNumber "012-1234-5678" of
        Left actual -> unwrapDomainError actual `shouldBe` "Invalid phone number format."
        Right _ -> expectationFailure "Expected an invalid PhoneNumber"

    it "should return an error for an input without hyphens" $ do
      case mkPhoneNumber "09012345678" of
        Left actual -> unwrapDomainError actual `shouldBe` "Invalid phone number format."
        Right _ -> expectationFailure "Expected an invalid PhoneNumber"

    it "should return an error for invalid hyphen placement" $ do
      case mkPhoneNumber "03--1234-5678" of
        Left actual -> unwrapDomainError actual `shouldBe` "Invalid phone number format."
        Right _ -> expectationFailure "Expected an invalid PhoneNumber"

    it "should return an error for an overly long phone number" $ do
      case mkPhoneNumber "090-1234-56789" of
        Left actual -> unwrapDomainError actual `shouldBe` "Invalid phone number format."
        Right _ -> expectationFailure "Expected an invalid PhoneNumber"

    it "should return an error for empty input" $ do
      case mkPhoneNumber "" of
        Left actual -> unwrapDomainError actual `shouldBe` "Phone number cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid PhoneNumber"

    it "should return an error for whitespace-only input" $ do
      case mkPhoneNumber "    " of
        Left actual -> unwrapDomainError actual `shouldBe` "Phone number cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid PhoneNumber"
