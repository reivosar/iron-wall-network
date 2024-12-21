{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.PhoneTypeSpec (spec) where

import Domain.BankAccount.ValueObject.PhoneType
import Domain.ValueError (mkValueError)
import Test.Hspec

spec :: Spec
spec = do
  describe "phoneTypeToText" $ do
    it "should convert Mobile to 'mobile'" $ do
      phoneTypeToText Mobile `shouldBe` "mobile"

    it "should convert Home to 'home'" $ do
      phoneTypeToText Home `shouldBe` "home"

    it "should convert Work to 'work'" $ do
      phoneTypeToText Work `shouldBe` "work"

  describe "textToPhoneType" $ do
    it "should parse 'mobile' to Mobile" $ do
      textToPhoneType "mobile" `shouldBe` Right Mobile

    it "should parse 'home' to Home" $ do
      textToPhoneType "home" `shouldBe` Right Home

    it "should parse 'work' to Work" $ do
      textToPhoneType "work" `shouldBe` Right Work

    it "should return an error for invalid input" $ do
      textToPhoneType "invalid" `shouldBe` Left (mkValueError "Invalid PhoneType. Expected 'mobile', 'home', or 'work'.")
