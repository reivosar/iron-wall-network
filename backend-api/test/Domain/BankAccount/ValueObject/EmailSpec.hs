{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.EmailSpec (spec) where

import Domain.BankAccount.ValueObject.Email
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkEmail" $ do
    it "should create an Email for a valid email address" $ do
      case mkEmail "user@example.com" of
        Right email -> unwrapEmail email `shouldBe` "user@example.com"
        Left _ -> expectationFailure "Expected a valid Email"

    it "should trim whitespace from input" $ do
      case mkEmail "  user@example.com  " of
        Right email -> unwrapEmail email `shouldBe` "user@example.com"
        Left _ -> expectationFailure "Expected a valid Email"

    it "should return an error for an empty input" $ do
      case mkEmail "" of
        Left err -> unwrapDomainError err `shouldBe` "Email cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid Email"

    it "should return an error for whitespace-only input" $ do
      case mkEmail "   " of
        Left err -> unwrapDomainError err `shouldBe` "Email cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid Email"

    it "should return an error for an invalid email address without @ symbol" $ do
      case mkEmail "userexample.com" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid email format."
        Right _ -> expectationFailure "Expected an invalid Email"

    it "should return an error for an invalid email address without domain" $ do
      case mkEmail "user@" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid email format."
        Right _ -> expectationFailure "Expected an invalid Email"

    it "should return an error for an invalid email address with invalid domain" $ do
      case mkEmail "user@example" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid email format."
        Right _ -> expectationFailure "Expected an invalid Email"

    it "should return an error for an email with invalid characters" $ do
      case mkEmail "user@exa mple.com" of
        Left err -> unwrapDomainError err `shouldBe` "Invalid email format."
        Right _ -> expectationFailure "Expected an invalid Email"

    it "should create an Email for a valid complex email address" $ do
      case mkEmail "user.name+tag+sorting@example.com" of
        Right email -> unwrapEmail email `shouldBe` "user.name+tag+sorting@example.com"
        Left _ -> expectationFailure "Expected a valid Email"
