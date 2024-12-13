module Domain.BankAccount.ValueObject.PhoneNumber
  ( PhoneNumber,
    mkPhoneNumber,
    unwrapPhoneNumber,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError (..))
import Text.Regex.TDFA ((=~))

newtype PhoneNumber = PhoneNumber {unwrapPhoneNumber :: Text}
  deriving (Show, Eq)

mkPhoneNumber :: Text -> Either ValueError PhoneNumber
mkPhoneNumber input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ ValueError "Phone number cannot be empty or whitespace."
        else
          if not (T.unpack trimmedInput =~ phoneNumberRegex)
            then Left $ ValueError "Invalid phone number format."
            else Right $ PhoneNumber trimmedInput

phoneNumberRegex :: String
phoneNumberRegex = "^[+]?\\d{1,15}$"
