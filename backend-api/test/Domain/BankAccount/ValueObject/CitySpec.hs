{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.CitySpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.City
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkCity" $ do
    it "should create a City for valid input" $ do
      case mkCity "Valid City Name" of
        Right city -> unwrapCity city `shouldBe` "Valid City Name"
        Left _ -> expectationFailure "Expected a valid City"

    it "should trim whitespace from input" $ do
      case mkCity "  Valid City Name  " of
        Right city -> unwrapCity city `shouldBe` "Valid City Name"
        Left _ -> expectationFailure "Expected a valid City"

    it "should return an error for empty input" $ do
      case mkCity "" of
        Left err -> unwrapDomainError err `shouldBe` "City cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid City"

    it "should return an error for whitespace-only input" $ do
      case mkCity "    " of
        Left err -> unwrapDomainError err `shouldBe` "City cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid City"

    it "should create a City for 255 characters" $ do
      let input = T.replicate 255 "a"
      case mkCity input of
        Right city -> unwrapCity city `shouldBe` input
        Left _ -> expectationFailure "Expected a valid City"

    it "should return an error for input longer than 255 characters" $ do
      let input = T.replicate 256 "a"
      case mkCity input of
        Left err -> unwrapDomainError err `shouldBe` "City cannot exceed 255 characters (got 256)."
        Right _ -> expectationFailure "Expected an invalid City"
