{-# LANGUAGE OverloadedStrings #-}

module Utils.Validation
  ( validateRegex,
    validateNotEmpty,
    validateMaxLength,
    validateMinLength,
    validateLengthRange,
    validateAllowedCharacters,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

validateRegex :: Text -> String -> Bool
validateRegex input pattern = T.unpack input =~ pattern

validateNotEmpty :: Text -> Bool
validateNotEmpty input = not $ T.null $ T.strip input

validateMaxLength :: Int -> Text -> Bool
validateMaxLength maxLength input = T.length input <= maxLength

validateMinLength :: Int -> Text -> Bool
validateMinLength minLength input = T.length input >= minLength

validateLengthRange :: Int -> Int -> Text -> Bool
validateLengthRange minLength maxLength input =
  T.length input >= minLength && T.length input <= maxLength

validateAllowedCharacters :: Text -> Text -> Bool
validateAllowedCharacters input allowedChars = T.all (`T.elem` allowedChars) input
