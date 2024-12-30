{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.TokenValidatorSpec (spec) where

import Application.Auth.Services.AuthService (AuthService, AuthServiceError (..))
import qualified Application.Auth.Services.AuthService as AuthService
import Application.TokenInvalidationError
  ( createExpiredTokenError,
    createInvalidTokenError,
    createUnknownError,
  )
import Application.TokenValidator as TokenValidator
import Data.Text (Text)
import Test.Hspec

-- Mock AuthService environment
data MockAuthService = MockAuthService
  { mockValidateToken :: Text -> IO (Either AuthServiceError ())
  }

instance AuthService IO where
  validateToken token = mockValidateToken mockEnv token
  findUserByUsername = undefined
  createAccessToken = undefined
  recreateAccessToken = undefined
  findAccessTokenByAccessToken = undefined
  findRefreshTokenByRefreshToken = undefined
  invalidateToken = undefined

-- Mock environment
mockEnv :: MockAuthService
mockEnv =
  MockAuthService
    { mockValidateToken = \token ->
        case token of
          "validToken" -> pure $ Right ()
          "expiredToken" -> pure $ Left (TokenExpiredError "Token expired")
          "invalidToken" -> pure $ Left (TokenNotFoundError "Token not found")
          _ -> pure $ Left (DatabaseError "Database error")
    }

spec :: Spec
spec = do
  describe "validateToken" $ do
    it "should validate a valid token successfully" $ do
      let token = "validToken"
      result <- TokenValidator.validateToken token
      result `shouldBe` Right ()

    it "should return an error for an expired token" $ do
      let token = "expiredToken"
      result <- TokenValidator.validateToken token
      result `shouldBe` Left (createExpiredTokenError "Token expired")

    it "should return an error for an invalid token" $ do
      let token = "invalidToken"
      result <- TokenValidator.validateToken token
      result `shouldBe` Left (createInvalidTokenError "Token not found")

    it "should return an unknown error for a database error" $ do
      let token = "unknownToken"
      result <- TokenValidator.validateToken token
      result `shouldBe` Left (createUnknownError "Database error")

  describe "mapAuthServiceErrorToTokenInvalidationError" $ do
    it "should map TokenNotFoundError to createInvalidTokenError" $ do
      let err = AuthService.TokenNotFoundError "Token not found"
      let result = TokenValidator.mapAuthServiceErrorToTokenInvalidationError err
      result `shouldBe` createInvalidTokenError "Token not found"

    it "should map TokenExpiredError to createExpiredTokenError" $ do
      let err = AuthService.TokenExpiredError "Token expired"
      let result = TokenValidator.mapAuthServiceErrorToTokenInvalidationError err
      result `shouldBe` createExpiredTokenError "Token expired"

    it "should map DatabaseError to createUnknownError" $ do
      let err = AuthService.DatabaseError "Database error"
      let result = TokenValidator.mapAuthServiceErrorToTokenInvalidationError err
      result `shouldBe` createUnknownError "Database error"

    it "should map unexpected errors to createUnknownError" $ do
      let err = AuthService.TokenGenerationError "Unexpected error"
      let result = TokenValidator.mapAuthServiceErrorToTokenInvalidationError err
      result `shouldBe` createUnknownError "Unexpected error occurred during token validation"
