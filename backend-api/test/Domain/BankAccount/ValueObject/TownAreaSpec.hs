{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.TownAreaSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.TownArea
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkTownArea" $ do
    it "should create a TownArea for valid input" $ do
      case mkTownArea "Valid Town Area" of
        Right townArea -> unwrapTownArea townArea `shouldBe` "Valid Town Area"
        Left _ -> expectationFailure "Expected a valid TownArea"

    it "should trim whitespace from input" $ do
      case mkTownArea "  Valid Town Area  " of
        Right townArea -> unwrapTownArea townArea `shouldBe` "Valid Town Area"
        Left _ -> expectationFailure "Expected a valid TownArea"

    it "should return an error for empty input" $ do
      case mkTownArea "" of
        Left err -> unwrapDomainError err `shouldBe` "Town/Area cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid TownArea"

    it "should return an error for whitespace-only input" $ do
      case mkTownArea "    " of
        Left err -> unwrapDomainError err `shouldBe` "Town/Area cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid TownArea"

    it "should create a TownArea for 255 characters" $ do
      let input = T.replicate 255 "a"
      case mkTownArea input of
        Right townArea -> unwrapTownArea townArea `shouldBe` input
        Left _ -> expectationFailure "Expected a valid TownArea"

    it "should return an error for input longer than 255 characters" $ do
      let input = T.replicate 256 "a"
      case mkTownArea input of
        Left err -> unwrapDomainError err `shouldBe` "Town/Area cannot exceed 255 characters (got 256)."
        Right _ -> expectationFailure "Expected an invalid TownArea"
