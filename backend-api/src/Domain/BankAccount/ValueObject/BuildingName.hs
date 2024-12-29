{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.BuildingName
  ( BuildingName,
    mkBuildingName,
    unwrapBuildingName,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Domain.ValueError (ValueError, mkValueError)

newtype BuildingName = BuildingName {unwrapBuildingName :: Text}
  deriving (Show, Eq)

mkBuildingName :: Text -> Either ValueError BuildingName
mkBuildingName input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ mkValueError "Building name cannot be empty or whitespace."
        else
          if T.length trimmedInput > 255
            then Left $ mkValueError $ "Building name cannot exceed 255 characters (got " <> pack (show (T.length trimmedInput)) <> ")."
            else Right $ BuildingName trimmedInput
