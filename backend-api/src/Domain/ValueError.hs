{-# LANGUAGE DeriveGeneric #-}

module Domain.ValueError
  ( ValueError,
    mkValueError,
    unwrapValueError,
  )
where

import Data.Text (Text)

newtype ValueError = ValueError {unwrapValueError :: Text}
  deriving (Show, Eq)

mkValueError :: Text -> ValueError
mkValueError msg = ValueError msg
