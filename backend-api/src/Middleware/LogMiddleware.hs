{-# LANGUAGE OverloadedStrings #-}

module Middleware.LogMiddleware (logMiddleware) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (KeyValue, Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (Key, fromText, toText)
import Data.Aeson.KeyMap (KeyMap, mapWithKey, toList)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (atomicModifyIORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Infrastructure.Repositories.PostgresAuditLogRepository
import Middleware.AuditLogRepository (AuditLog (..), AuditLogRepository (..))
import Network.HTTP.Types (hContentType, statusCode)
import Network.Wai
import Network.Wai (Response, responseToStream)
import qualified Utils.Logger as Logger
import Utils.UUIDGenerator

logMiddleware :: Middleware
logMiddleware app originalRequest sendResponse = do
  startTime <- liftIO getCurrentTime
  transactionId <- liftIO generateUUID
  generateResult <- generateId
  (req, bodyChunks) <- cacheRequestBody originalRequest
  parameters <- encodeParameters bodyChunks

  let query = encodeQuery (queryString req)
      ipAddress = fmap decodeUtf8 (getClientIP req)
      userAgent = fmap decodeUtf8 (lookup "User-Agent" (requestHeaders req))
      contentType = fmap decodeUtf8 (lookup "Content-Type" (requestHeaders req))

  case generateResult of
    Left err -> do
      Logger.logError $
        "[TraceID: N/A] Failed to generate ID: " ++ show err
      app req sendResponse
    Right id -> do
      Logger.logInfo $
        "[TraceID: "
          ++ show id
          ++ "] Request: "
          ++ show (requestMethod req)
          ++ " "
          ++ show (rawPathInfo req)
          ++ ", Body: "
          ++ show parameters
          ++ ", Query: "
          ++ show query

      app req $ \response -> do
        endTime <- liftIO getCurrentTime
        responseMessage <- extractResponseMessage response

        let auditLog =
              AuditLog
                { auditLogId = id,
                  transactionId = transactionId,
                  operatorId = Nothing,
                  ipAddress = ipAddress,
                  userAgent = userAgent,
                  description = Nothing,
                  url = decodeUtf8 (rawPathInfo req),
                  contentType = contentType,
                  method = pack (show (requestMethod req)),
                  parameters = parameters,
                  queryText = query,
                  responseStatusCode = Just (statusCode (responseStatus response)),
                  responseMessage = Just $ responseMessage,
                  requestStartedAt = startTime,
                  requestEndedAt = Just endTime
                }

        Logger.logInfo $ "[TraceID: " ++ show id ++ "] Response: " ++ show (responseStatus response)

        saveResult <- save auditLog
        case saveResult of
          Left saveErr ->
            Logger.logError $
              "[TraceID: " ++ show id ++ "] Failed to save audit log: " ++ show saveErr
          Right _ -> return ()

        sendResponse response

cacheRequestBody :: Request -> IO (Request, [BS.ByteString])
cacheRequestBody req = do
  let loop front = do
        bs <- getRequestBodyChunk req
        if BS.null bs
          then return $ front []
          else loop $ front . (bs :)
  body <- loop id
  ichunks <- newIORef body
  let rbody = atomicModifyIORef ichunks $ \chunks ->
        case chunks of
          [] -> ([], BS.empty)
          x : y -> (y, x)
  let req' = setRequestBodyChunks rbody req
  return (req', body)

encodeParameters :: [BS.ByteString] -> IO (Maybe Text)
encodeParameters chunks = do
  let body = BS.concat chunks
      decodedBody :: Maybe Value
      decodedBody = decode (BSL.fromStrict body)
      sanitizedBody = fmap maskSensitiveData decodedBody
      encodedBody =
        case sanitizedBody of
          Just obj
            | obj == object [] -> Nothing
            | otherwise -> Just $ decodeUtf8 $ BSL.toStrict $ encode obj
          Nothing -> Nothing
  return encodedBody

encodeQuery :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Text
encodeQuery queryParams =
  let params =
        map (\(k, v) -> fromText (decodeUtf8 k) .= decodeUtf8 (fromMaybe "" v)) queryParams
   in if null params
        then Nothing
        else Just $ decodeUtf8 $ BSL.toStrict $ encode $ object params

sensitiveKeys :: [Text]
sensitiveKeys = ["password", "authKey"]

maskSensitiveData :: Value -> Value
maskSensitiveData (Object obj) = object $ map maskKey (toList obj)
  where
    maskKey :: (Key, Value) -> (Key, Value)
    maskKey (k, v)
      | toText k `elem` sensitiveKeys = (k, String "*****")
      | otherwise = (k, maskSensitiveData v)
maskSensitiveData (Array arr) = Array (fmap maskSensitiveData arr)
maskSensitiveData other = other

fromKey :: Key -> Text
fromKey = Data.Aeson.Key.toText

getClientIP :: Request -> Maybe BS.ByteString
getClientIP req =
  lookup "X-Forwarded-For" (requestHeaders req) <|> lookup "X-Real-IP" (requestHeaders req)

extractResponseMessage :: Response -> IO Text
extractResponseMessage response = do
  let code = statusCode $ responseStatus response
  if code >= 400
    then do
      body <- getResponseBody response
      return $ pack $ unpack (decodeUtf8 (BSL.toStrict body))
    else return $ pack $ "Success"
  where
    unpack = Data.Text.unpack

getResponseBody :: Response -> IO ByteString
getResponseBody res =
  let (status, headers, body) = responseToStream res
   in body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content
