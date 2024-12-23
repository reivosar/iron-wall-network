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

createInvalidTokenError :: Text -> TokenInvalidationError
createInvalidTokenError msg = InvalidToken msg

createExpiredTokenError :: Text -> TokenInvalidationError
createExpiredTokenError msg = ExpiredToken msg

createUnknownError :: Text -> TokenInvalidationError
createUnknownError msg = UnknownError msg
