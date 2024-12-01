{-# LANGUAGE OverloadedStrings #-}

module Application.Auth.TokenValidator where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Web.JWT (JWT, claims, decode, unregisteredClaims)

validateToken :: Text -> IO (Either UseCaseError ())
validateToken token = do
  return $ Right ()
