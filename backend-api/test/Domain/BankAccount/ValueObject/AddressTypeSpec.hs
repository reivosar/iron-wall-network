{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.AddressTypeSpec (spec) where

import Domain.BankAccount.ValueObject.AddressType
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "textToAddressType" $ do
    it "should return Home for 'home'" $ do
      textToAddressType "home" `shouldBe` Right Home

    it "should return Office for 'office'" $ do
      textToAddressType "office" `shouldBe` Right Office

    it "should be case insensitive" $ do
      textToAddressType "hOmE" `shouldBe` Right Home
      textToAddressType "OfFiCe" `shouldBe` Right Office

    it "should return an error for invalid input" $ do
      let invalidInputs = ["invalid", "123", "HomeBilling"]
      mapM_
        ( \input ->
            case textToAddressType input of
              Left err -> unwrapDomainError err `shouldBe` "Invalid AddressType. Expected 'home', or 'office'."
              Right _ -> expectationFailure $ "Expected an invalid AddressType for: " ++ show input
        )
        invalidInputs

    it "should return an error for empty input" $ do
      case textToAddressType "" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid AddressType. Expected 'home', or 'office'."
        Right _ -> expectationFailure "Expected an invalid AddressType"

  describe "addressTypeToText" $ do
    it "should return 'home' for Home" $ do
      addressTypeToText Home `shouldBe` "home"

    it "should return 'office' for Office" $ do
      addressTypeToText Office `shouldBe` "office"
