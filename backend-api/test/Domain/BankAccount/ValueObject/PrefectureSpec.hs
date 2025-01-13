{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.PrefectureSpec (spec) where

import qualified Data.Text as T
import Domain.BankAccount.ValueObject.Prefecture
import Domain.Error (unwrapDomainError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkPrefecture" $ do
    it "should create a Prefecture for all valid prefectures" $ do
      mapM_
        ( \prefecture ->
            case mkPrefecture prefecture of
              Right p -> unwrapPrefecture p `shouldBe` prefecture
              Left _ -> expectationFailure $ "Expected a valid Prefecture for: " ++ T.unpack prefecture
        )
        validPrefectures

    it "should trim whitespace from valid prefectures" $ do
      mapM_
        ( \prefecture ->
            case mkPrefecture ("  " <> prefecture <> "  ") of
              Right p -> unwrapPrefecture p `shouldBe` prefecture
              Left _ -> expectationFailure $ "Expected a valid Prefecture for: " ++ T.unpack prefecture
        )
        validPrefectures

    it "should return an error for empty input" $ do
      case mkPrefecture "" of
        Left err -> unwrapDomainError err `shouldBe` "Prefecture cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid Prefecture"

    it "should return an error for whitespace-only input" $ do
      case mkPrefecture "    " of
        Left err -> unwrapDomainError err `shouldBe` "Prefecture cannot be empty or whitespace."
        Right _ -> expectationFailure "Expected an invalid Prefecture"

    it "should return an error for invalid prefectures" $ do
      let invalidInputs = ["InvalidPrefecture", "東京都Extra", "1234", "北海道!"]
      mapM_
        ( \input ->
            case mkPrefecture input of
              Left err -> unwrapDomainError err `shouldBe` ("Invalid prefecture: " <> input)
              Right _ -> expectationFailure $ "Expected an invalid Prefecture for: " ++ T.unpack input
        )
        invalidInputs
