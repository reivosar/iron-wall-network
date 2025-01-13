{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.PhoneType
  ( PhoneType (..),
    textToPhoneType,
    phoneTypeToText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (DomainError, mkDomainError)

data PhoneType = Mobile | Home | Work
  deriving (Show, Eq)

phoneTypeToText :: PhoneType -> Text
phoneTypeToText Mobile = T.pack "mobile"
phoneTypeToText Home = T.pack "home"
phoneTypeToText Work = T.pack "work"

textToPhoneType :: Text -> Either DomainError PhoneType
textToPhoneType txt =
  case T.unpack (T.toLower txt) of
    "mobile" -> Right Mobile
    "home" -> Right Home
    "work" -> Right Work
    _ -> Left $ mkDomainError "Invalid PhoneType. Expected 'mobile', 'home', or 'work'."
