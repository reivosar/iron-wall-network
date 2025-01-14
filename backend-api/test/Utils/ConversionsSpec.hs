{-# LANGUAGE OverloadedStrings #-}

module Utils.ConversionsSpec (spec) where

import Test.Hspec
import Utils.Conversions

spec :: Spec
spec = do
  describe "eitherToMaybe" $ do
    it "should convert Right value to Just" $ do
      eitherToMaybe (Right 42 :: Either String Int) `shouldBe` Just 42

    it "should convert Left value to Nothing" $ do
      eitherToMaybe (Left "error" :: Either String Int) `shouldBe` Nothing

  describe "maybeToEither" $ do
    it "should convert Just value to Right" $ do
      maybeToEither ("default" :: String) (Just 42 :: Maybe Int) `shouldBe` Right 42

    it "should convert Nothing to Left with the provided value" $ do
      maybeToEither ("default" :: String) (Nothing :: Maybe Int) `shouldBe` Left "default"

  describe "maximumByMay" $ do
    it "should return the maximum element using the comparator" $ do
      maximumByMay compare ([1, 3, 2] :: [Int]) `shouldBe` Just 3

    it "should return Nothing for an empty list" $ do
      maximumByMay compare ([] :: [Int]) `shouldBe` Nothing

  describe "safeHead" $ do
    it "should return the first element of a non-empty list" $ do
      safeHead ([1, 2, 3] :: [Int]) `shouldBe` Just 1

    it "should return Nothing for an empty list" $ do
      safeHead ([] :: [Int]) `shouldBe` Nothing

  describe "safeLast" $ do
    it "should return the last element of a non-empty list" $ do
      safeLast ([1, 2, 3] :: [Int]) `shouldBe` Just 3

    it "should return Nothing for an empty list" $ do
      safeLast ([] :: [Int]) `shouldBe` Nothing

  describe "mapMaybe" $ do
    it "should apply the function and filter out Nothing values" $ do
      mapMaybe (\x -> if x > 2 then Just x else Nothing) ([1, 2, 3, 4] :: [Int]) `shouldBe` [3, 4]

    it "should return an empty list if no elements pass the filter" $ do
      mapMaybe (\x -> if x > 5 then Just x else Nothing) ([1, 2, 3] :: [Int]) `shouldBe` []

  describe "isRight" $ do
    it "should return True for Right value" $ do
      isRight (Right 42 :: Either String Int) `shouldBe` True

    it "should return False for Left value" $ do
      isRight (Left "error" :: Either String Int) `shouldBe` False

  describe "isLeft" $ do
    it "should return True for Left value" $ do
      isLeft (Left "error" :: Either String Int) `shouldBe` True

    it "should return False for Right value" $ do
      isLeft (Right 42 :: Either String Int) `shouldBe` False

  describe "fromRight" $ do
    it "should return the Right value" $ do
      fromRight 0 (Right 42 :: Either String Int) `shouldBe` 42

    it "should return the default value for Left" $ do
      fromRight 0 (Left "error" :: Either String Int) `shouldBe` 0

  describe "fromLeft" $ do
    it "should return the Left value" $ do
      fromLeft "default" (Left "error" :: Either String Int) `shouldBe` "error"

    it "should return the default value for Right" $ do
      fromLeft "default" (Right 42 :: Either String Int) `shouldBe` "default"

  describe "onJust" $ do
    it "should apply the function for Just value" $ do
      onJust (Just 42 :: Maybe Int) (* 2) `shouldBe` Just 84

    it "should return Nothing for Nothing" $ do
      onJust (Nothing :: Maybe Int) (* 2) `shouldBe` Nothing

  describe "listToMaybe" $ do
    it "should convert a non-empty list to Just (first element)" $ do
      listToMaybe ([1, 2, 3] :: [Int]) `shouldBe` Just 1

    it "should return Nothing for an empty list" $ do
      listToMaybe ([] :: [Int]) `shouldBe` Nothing

  describe "maybeToList" $ do
    it "should convert Just value to a singleton list" $ do
      maybeToList (Just 42 :: Maybe Int) `shouldBe` [42]

    it "should return an empty list for Nothing" $ do
      maybeToList (Nothing :: Maybe Int) `shouldBe` []
