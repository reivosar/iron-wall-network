{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.AddressType
  ( AddressType (..),
    textToAddressType,
    addressTypeToText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError, mkValueError)

data AddressType = Home | Office
  deriving (Show, Eq)

addressTypeToText :: AddressType -> Text
addressTypeToText Home = T.pack "home"
addressTypeToText Office = T.pack "office"

textToAddressType :: Text -> Either ValueError AddressType
textToAddressType txt =
  case T.unpack (T.toLower txt) of
    "home" -> Right Home
    "office" -> Right Office
    _ -> Left $ mkValueError "Invalid AddressType. Expected 'home', or 'office'."
