{-# LANGUAGE DeriveGeneric #-}

module Domain.ValueError (ValueError (..), formatError) where

import Data.Text (Text, pack)
import GHC.Generics (Generic)

newtype ValueError = ValueError String
  deriving (Show, Eq, Generic)

formatError :: ValueError -> Text
formatError (ValueError msg) = pack msg
