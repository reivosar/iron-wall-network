{-# LANGUAGE DeriveGeneric #-}

module Domain.ValueError
  ( ValueError,
    mkValueError,
    valueErrorToText,
    unwrapValueError,
  )
where

import Data.Text (Text, pack)
import GHC.Generics (Generic)

newtype ValueError = ValueError {unwrapValueError :: String}
  deriving (Show, Eq)

mkValueError :: String -> ValueError
mkValueError msg = ValueError msg

valueErrorToText :: ValueError -> Text
valueErrorToText (ValueError msg) = pack msg
