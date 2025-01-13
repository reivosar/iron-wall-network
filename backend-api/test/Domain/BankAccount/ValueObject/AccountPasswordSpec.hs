{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.AccountPasswordSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.AccountPassword
import Domain.Error (unwrapDomainError)
import Test.Hspec
import Utils.HashGenerator (generateHMAC)

spec :: Spec
spec = do
  let secretKey = "test-secret-key"

  describe "mkAccountPassword" $ do
    it "should return an error if the password is empty" $ do
      let actual = mkAccountPassword "" secretKey
      case actual of
        Left err -> unwrapDomainError err `shouldBe` "Password cannot be empty."
        Right _ -> expectationFailure "Expected a validation error for empty password"

    it "should create an AccountPassword if the password is valid" $ do
      let actual = mkAccountPassword "valid-password" secretKey
      case actual of
        Right password -> unwrapPasswordHash password `shouldNotBe` T.empty
        Left _ -> expectationFailure "Expected a valid AccountPassword"

  describe "mkHashedAccountPassword" $ do
    it "should return an error if the hashed password is empty" $ do
      let actual = mkHashedAccountPassword ""
      case actual of
        Left err -> unwrapDomainError err `shouldBe` "Hashed password cannot be empty."
        Right _ -> expectationFailure "Expected a validation error for empty hashed password"

    it "should create an AccountPassword if the hashed password is valid" $ do
      let actual = mkHashedAccountPassword "hashed-password"
      case actual of
        Right password -> unwrapPasswordHash password `shouldBe` "hashed-password"
        Left _ -> expectationFailure "Expected a valid AccountPassword"

  describe "verifyPassword" $ do
    it "should return False if the passwords do not match" $ do
      let accountPassword1 = mkHashedAccountPassword $ generateHMAC "valid-password" secretKey
      let accountPassword2 = mkHashedAccountPassword $ generateHMAC "wrong-password" secretKey
      case (accountPassword1, accountPassword2) of
        (Right pwd1, Right pwd2) -> verifyPassword pwd1 pwd2 `shouldBe` False
        _ -> expectationFailure "Expected valid AccountPasswords for verification"

    it "should return True if the passwords match" $ do
      let accountPassword1 = mkHashedAccountPassword $ generateHMAC "valid-password" secretKey
      let accountPassword2 = mkHashedAccountPassword $ generateHMAC "valid-password" secretKey
      case (accountPassword1, accountPassword2) of
        (Right pwd1, Right pwd2) -> verifyPassword pwd1 pwd2 `shouldBe` True
        _ -> expectationFailure "Expected valid AccountPasswords for verification"
