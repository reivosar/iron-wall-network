{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.AccountPasswordSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.AccountPassword
import Domain.ValueError (mkValueError)
import Test.Hspec
import Utils.HashGenerator (generateHMAC)

spec :: Spec
spec = do
  let secretKey = "test-secret-key"

  describe "mkAccountPassword" $ do
    it "should return an error if the password is empty" $ do
      mkAccountPassword "" secretKey `shouldBe` Left (mkValueError "Password cannot be empty.")

    it "should create an AccountPassword if the password is valid" $ do
      let Right password = mkAccountPassword "valid-password" secretKey
      unwrapPasswordHash password `shouldNotBe` T.empty

  describe "mkHashedAccountPassword" $ do
    it "should return an error if the hashed password is empty" $ do
      mkHashedAccountPassword "" `shouldBe` Left (mkValueError "Hashed password cannot be empty.")

    it "should create an AccountPassword if the hashed password is valid" $ do
      let Right password = mkHashedAccountPassword "hashed-password"
      unwrapPasswordHash password `shouldBe` "hashed-password"

  describe "verifyPassword" $ do
    it "should return False if the passwords do not match" $ do
      let Right accountPassword1 = mkHashedAccountPassword $ generateHMAC "valid-password" secretKey
      let Right accountPassword2 = mkHashedAccountPassword $ generateHMAC "wrong-password" secretKey
      verifyPassword accountPassword1 accountPassword2 `shouldBe` False

    it "should return True if the passwords match" $ do
      let Right accountPassword1 = mkHashedAccountPassword $ generateHMAC "valid-password" secretKey
      let Right accountPassword2 = mkHashedAccountPassword $ generateHMAC "valid-password" secretKey
      verifyPassword accountPassword1 accountPassword2 `shouldBe` True
