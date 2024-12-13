{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.TokenInvalidationError
  ( TokenInvalidationError (..),
    createExpiredTokenError,
    createInvalidTokenError,
    createUnknownError,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data TokenInvalidationError
  = InvalidToken Text
  | ExpiredToken Text
  | UnknownError Text
  deriving (Show, Generic, ToJSON)

createInvalidTokenError :: String -> TokenInvalidationError
createInvalidTokenError msg = InvalidToken (pack msg)

createExpiredTokenError :: String -> TokenInvalidationError
createExpiredTokenError msg = ExpiredToken (pack msg)

createUnknownError :: String -> TokenInvalidationError
createUnknownError msg = UnknownError (pack msg)
