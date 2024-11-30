module Domain.BankAccount.ValueObject.Username (Username, mkUsername, unwrapUsername) where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError (..))

newtype Username = Username {unwrapUsername :: Text}
  deriving (Show, Eq)

mkUsername :: Text -> Either ValueError Username
mkUsername input =
  let trimmedInput = T.strip input
      actualLen = T.length trimmedInput
   in if T.null trimmedInput
        then Left $ ValueError "Username cannot be empty or whitespace."
        else
          if actualLen > 100
            then Left $ ValueError $ "Username cannot exceed 100 characters (got " ++ show actualLen ++ ")."
            else Right $ Username trimmedInput
