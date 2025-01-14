{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.AggregateId
  ( AggregateId,
    mkAggregateId,
    unwrapAggregateId,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

newtype AggregateId = AggregateId {unwrapAggregateId :: Text}
  deriving (Show, Eq, Generic)

mkAggregateId :: Text -> AggregateId
mkAggregateId = AggregateId
