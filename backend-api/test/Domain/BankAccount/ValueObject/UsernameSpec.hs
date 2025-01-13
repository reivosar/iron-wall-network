{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.UsernameSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.Username
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkUsername" $ do
    it "should create a Username for valid input" $ do
      case mkUsername "validUsername" of
        Right username -> unwrapUsername username `shouldBe` "validUsername"
        Left _ -> expectationFailure "Expected a valid Username"

    it "should trim whitespace from input" $ do
      case mkUsername "  validUsername  " of
        Right username -> unwrapUsername username `shouldBe` "validUsername"
        Left _ -> expectationFailure "Expected a valid Username"

    it "should return an error for empty input" $ do
      case mkUsername "" of
        Left err -> unwrapDomainError err `shouldBe` "Username cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid Username"

    it "should return an error for whitespace-only input" $ do
      case mkUsername "   " of
        Left err -> unwrapDomainError err `shouldBe` "Username cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid Username"

    it "should create a Username for 100 characters" $ do
      let input = T.replicate 100 "1"
      case mkUsername input of
        Right username -> unwrapUsername username `shouldBe` input
        Left _ -> expectationFailure "Expected a valid Username"

    it "should return an error for input longer than 100 characters" $ do
      let input = T.replicate 101 "a"
      case mkUsername input of
        Left err -> unwrapDomainError err `shouldBe` "Username cannot exceed 100 characters (got 101)."
        Right _ -> expectationFailure "Expected an invalid Username"
