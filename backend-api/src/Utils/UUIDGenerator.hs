module Utils.UUIDGenerator (generateUUID, generateUUIDText) where

import Data.Text (Text)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)

generateUUID :: IO UUID
generateUUID = nextRandom

generateUUIDText :: IO Text
generateUUIDText = toText <$> nextRandom
