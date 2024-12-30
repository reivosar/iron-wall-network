{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Middleware.LogMiddlewareHelpersSpec (spec) where

import Control.Exception (SomeException, displayException)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Middleware.LogMiddlewareHelpers
import Middleware.OperatorIdService (OperatorIdService (..))
import qualified Network.HTTP.Types as HTTP
import Network.Wai (defaultRequest, requestHeaders, responseLBS)
import Test.Hspec

-- Mock environment
data MockOperatorIdService = MockOperatorIdService
  { mockGetOperatorIdByUserName :: Text -> IO (Either SomeException (Maybe Int)),
    mockGetOperatorIdByAccessToken :: Text -> IO (Either SomeException (Maybe Int)),
    mockGetOperatorIdByRefreshToken :: Text -> IO (Either SomeException (Maybe Int))
  }

instance OperatorIdService IO where
  getOperatorIdByUserName = mockGetOperatorIdByUserName mockEnv
  getOperatorIdByAccessToken = mockGetOperatorIdByAccessToken mockEnv
  getOperatorIdByRefreshToken = mockGetOperatorIdByRefreshToken mockEnv

-- Global mock environment
mockEnv :: MockOperatorIdService
mockEnv =
  MockOperatorIdService
    { mockGetOperatorIdByUserName = \_ -> pure (Right (Just 123)),
      mockGetOperatorIdByAccessToken = \_ -> pure (Right (Just 456)),
      mockGetOperatorIdByRefreshToken = \_ -> pure (Right (Just 789))
    }

spec :: Spec
spec = do
  describe "cacheRequestBody" $ do
    it "should return request with cached body" $ do
      let request = defaultRequest
      (_, bodyChunks) <- cacheRequestBody request
      bodyChunks `shouldBe` []

  describe "encodeParameters" $ do
    it "should encode valid JSON body" $ do
      let chunks = [BS.pack "{\"key\":\"value\"}"]
      result <- encodeParameters chunks
      result `shouldBe` Just "{\"key\":\"value\"}"

    it "should return Nothing for empty body" $ do
      let chunks = []
      result <- encodeParameters chunks
      result `shouldBe` Nothing

    it "should mask sensitive data" $ do
      let chunks = [BS.pack "{\"password\":\"1234\"}"]
      result <- encodeParameters chunks
      result `shouldBe` Just "{\"password\":\"*****\"}"

  describe "encodeQuery" $ do
    it "should encode query parameters" $ do
      let query = [("key", Just "value")]
      encodeQuery query `shouldBe` Just "{\"key\":\"value\"}"

    it "should return Nothing for empty query" $ do
      let query = []
      encodeQuery query `shouldBe` Nothing

  describe "maskSensitiveData" $ do
    it "should mask sensitive keys" $ do
      let input = object ["password" .= String "1234", "key" .= String "value"]
      let expected = object ["password" .= String "*****", "key" .= String "value"]
      maskSensitiveData input `shouldBe` expected

    it "should handle nested objects" $ do
      let input =
            object
              [ "key" .= object ["password" .= String "1234"],
                "key2" .= String "value"
              ]
      let expected =
            object
              [ "key" .= object ["password" .= String "*****"],
                "key2" .= String "value"
              ]
      maskSensitiveData input `shouldBe` expected

  describe "getClientIP" $ do
    it "should return X-Forwarded-For header if present" $ do
      let request = defaultRequest {requestHeaders = [("X-Forwarded-For", "127.0.0.1")]}
      getClientIP request `shouldBe` Just "127.0.0.1"

    it "should return X-Real-IP header if X-Forwarded-For is absent" $ do
      let request = defaultRequest {requestHeaders = [("X-Real-IP", "127.0.0.2")]}
      getClientIP request `shouldBe` Just "127.0.0.2"

    it "should return Nothing if no IP headers are present" $ do
      getClientIP defaultRequest `shouldBe` Nothing

  describe "getOperatorId" $ do
    it "should handle /auth/login path" $ do
      let params = Just "{\"userName\":\"testUser\"}"
      result <- getOperatorId "/v1/auth/login" params Nothing
      case result of
        Right (Just value) -> value `shouldBe` 123
        Right Nothing -> expectationFailure "Expected Just 123 but got Nothing"
        Left err -> expectationFailure $ "Expected success but got error: " <> displayException err

    it "should handle /auth/refresh path" $ do
      let params = Just "{\"refreshToken\":\"token\"}"
      result <- getOperatorId "/v1/auth/refresh" params Nothing
      case result of
        Right (Just value) -> value `shouldBe` 789
        Right Nothing -> expectationFailure "Expected Just 789 but got Nothing"
        Left err -> expectationFailure $ "Expected success but got error: " <> displayException err

    it "should handle accessToken" $ do
      let token = Just "accessToken"
      result <- getOperatorId "/v1/some/other/path" Nothing token
      case result of
        Right (Just value) -> value `shouldBe` 456
        Right Nothing -> expectationFailure "Expected Just 456 but got Nothing"
        Left err -> expectationFailure $ "Expected success but got error: " <> displayException err

  describe "extractFromParameters" $ do
    it "should extract a parameter from JSON body" $ do
      let params = "{\"key\":\"value\"}"
      extractFromParameters "key" params `shouldBe` Just "value"

    it "should return Nothing if key is not present" $ do
      let params = "{\"otherKey\":\"value\"}"
      extractFromParameters "key" params `shouldBe` Nothing

  describe "extractResponseMessage" $ do
    it "should return Success for 200 response" $ do
      let response = responseLBS HTTP.ok200 [] "OK"
      result <- extractResponseMessage response
      result `shouldBe` "Success"

    it "should return body for 400 response" $ do
      let response = responseLBS HTTP.badRequest400 [] "Error"
      result <- extractResponseMessage response
      result `shouldBe` "Error"

  describe "getResponseBody" $ do
    it "should extract response body" $ do
      let response = responseLBS HTTP.ok200 [] "Response Body"
      body <- getResponseBody response
      body `shouldBe` "Response Body"
