{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.FullNameSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.FullName
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkFullName" $ do
    it "should create a FullName for valid input" $ do
      case mkFullName "validFullName" of
        Right fullName -> unwrapFullName fullName `shouldBe` "validFullName"
        Left _ -> expectationFailure "Expected a valid FullName"

    it "should trim whitespace from input" $ do
      case mkFullName "  validFullName  " of
        Right fullName -> unwrapFullName fullName `shouldBe` "validFullName"
        Left _ -> expectationFailure "Expected a valid FullName"

    it "should return an error for empty input" $ do
      case mkFullName "" of
        Left err -> unwrapDomainError err `shouldBe` "Full name cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid FullName"

    it "should return an error for whitespace-only input" $ do
      case mkFullName "    " of
        Left err -> unwrapDomainError err `shouldBe` "Full name cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid FullName"

    it "should create a FullName for 255 characters" $ do
      let input = T.replicate 255 "a"
      case mkFullName input of
        Right fullName -> unwrapFullName fullName `shouldBe` input
        Left _ -> expectationFailure "Expected a valid FullName"

    it "should return an error for input longer than 255 characters" $ do
      let input = T.replicate 256 "a"
      case mkFullName input of
        Left err -> unwrapDomainError err `shouldBe` "Full name cannot exceed 255 characters (got 256)."
        Right _ -> expectationFailure "Expected an invalid FullName"
