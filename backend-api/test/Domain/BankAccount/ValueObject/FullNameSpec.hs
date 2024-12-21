{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.FullNameSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.FullName
import Domain.ValueError (mkValueError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkFullName" $ do
    it "should create a FullName for valid input" $ do
      mkFullName "validFullName" `shouldBe` mkFullName "validFullName"

    it "should trim whitespace from input" $ do
      mkFullName "  validFullName  " `shouldBe` mkFullName "validFullName"

    it "should return an error for empty input" $ do
      mkFullName "" `shouldBe` Left (mkValueError "Full name cannot be empty or whitespace.")

    it "should return an error for whitespace-only input" $ do
      mkFullName "    " `shouldBe` Left (mkValueError "Full name cannot be empty or whitespace.")

    it "should create a FullName for 255 characters" $ do
      let input = T.replicate 255 "a"
      mkFullName input `shouldBe` mkFullName input

    it "should return an error for input longer than 255 characters" $ do
      let input = T.replicate 256 "a"
      mkFullName input `shouldBe` Left (mkValueError "Full name cannot exceed 255 characters (got 256).")
