{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.CreateAccount where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

data CreateAccount = CreateAccount
  { username :: Text
  , fullName :: Text
  , email :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
