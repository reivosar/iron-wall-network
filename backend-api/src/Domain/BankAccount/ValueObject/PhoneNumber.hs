{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.PhoneNumber
  ( PhoneNumber,
    mkPhoneNumber,
    unwrapPhoneNumber,
    validatePhoneNumber,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError, mkValueError)
import Utils.Validation (validateRegex)

newtype PhoneNumber = PhoneNumber {unwrapPhoneNumber :: Text}
  deriving (Show, Eq)

mkPhoneNumber :: Text -> Either ValueError PhoneNumber
mkPhoneNumber input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ mkValueError "Phone number cannot be empty or whitespace."
        else
          if not (validatePhoneNumber trimmedInput)
            then Left $ mkValueError "Invalid phone number format."
            else Right $ PhoneNumber trimmedInput

validatePhoneNumber :: Text -> Bool
validatePhoneNumber input = validateRegex input phoneNumberRegex

phoneNumberRegex :: String
phoneNumberRegex =
  "^(0(3|4[1-9]|5[1-9]|6[1-9]|7[1-9]|8[1-9]|9[1-9])-[0-9]{3,4}-[0-9]{4})$"
    ++ "|^(0[1-9][0-9]{2}-[0-9]{2}-[0-9]{4})$"
    ++ "|^(0(70|80|90)-[0-9]{4}-[0-9]{4})$"
