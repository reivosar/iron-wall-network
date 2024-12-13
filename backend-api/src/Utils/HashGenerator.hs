{-# LANGUAGE OverloadedStrings #-}

module Utils.HashGenerator (generateHMAC) where

import Crypto.Hash (SHA512)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.ByteArray as BA (convert)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

generateHMAC :: T.Text -> T.Text -> T.Text
generateHMAC input key =
  let keyBytes = TE.encodeUtf8 key
      inputBytes = TE.encodeUtf8 input
      hmacDigest = hmac keyBytes inputBytes :: HMAC SHA512
   in TE.decodeUtf8 $ Base64.encode $ BA.convert $ hmacGetDigest hmacDigest
