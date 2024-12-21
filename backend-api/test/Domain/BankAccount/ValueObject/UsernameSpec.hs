{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.UsernameSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.Username
import Domain.ValueError (mkValueError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkUsername" $ do
    it "should create a Username for valid input" $ do
      mkUsername "validUsername" `shouldBe` mkUsername "validUsername"

    it "should trim whitespace from input" $ do
      mkUsername "  validUsername  " `shouldBe` mkUsername "validUsername"

    it "should return an error for empty input" $ do
      mkUsername "" `shouldBe` Left (mkValueError "Username cannot be empty or whitespace.")

    it "should return an error for whitespace-only input" $ do
      mkUsername "   " `shouldBe` Left (mkValueError "Username cannot be empty or whitespace.")

    it "should create a Username for 100 characters" $ do
      let inputlUsername = T.replicate 100 "1"
      let expectedUsername = T.replicate 100 "1"
      mkUsername inputlUsername `shouldBe` mkUsername expectedUsername

    it "should return an error for input longer than 255 characters" $ do
      let longUsername = T.replicate 101 "a"
      mkUsername longUsername `shouldBe` Left (mkValueError "Username cannot exceed 100 characters (got 101).")
