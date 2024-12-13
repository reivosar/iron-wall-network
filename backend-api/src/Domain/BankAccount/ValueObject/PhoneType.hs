module Domain.BankAccount.ValueObject.PhoneType
  ( PhoneType (..),
    phoneTypeToText,
    textToPhoneType,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError (..))

data PhoneType = Mobile | Home | Work
  deriving (Show, Eq)

phoneTypeToText :: PhoneType -> Text
phoneTypeToText Mobile = T.pack "mobile"
phoneTypeToText Home = T.pack "home"
phoneTypeToText Work = T.pack "work"

textToPhoneType :: Text -> Either ValueError PhoneType
textToPhoneType t
  | T.toLower t == T.pack "mobile" = Right Mobile
  | T.toLower t == T.pack "home" = Right Home
  | T.toLower t == T.pack "work" = Right Work
  | otherwise = Left $ ValueError $ "Invalid phone type"
