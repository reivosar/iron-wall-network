{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.TownArea
  ( TownArea,
    mkTownArea,
    unwrapTownArea,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Domain.Error (DomainError, mkDomainError)

newtype TownArea = TownArea {unwrapTownArea :: Text}
  deriving (Show, Eq)

mkTownArea :: Text -> Either DomainError TownArea
mkTownArea input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ mkDomainError "Town/Area cannot be empty or whitespace."
        else
          if T.length trimmedInput > 255
            then Left $ mkDomainError $ "Town/Area cannot exceed 255 characters (got " <> pack (show (T.length trimmedInput)) <> ")."
            else Right $ TownArea trimmedInput
