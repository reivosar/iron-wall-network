{-# LANGUAGE OverloadedStrings #-}

module Middleware.LogMiddleware (logMiddleware) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (Key, fromText, toText)
import qualified Data.Aeson.KeyMap as KM (lookup, toList)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
  ( atomicModifyIORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, isPrefixOf, pack, splitOn, stripPrefix, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Infrastructure.Services.PostgresAuditLogService
import Infrastructure.Services.PostgresOperatorIdService
import Middleware.AuditLogService (AuditLog (..))
import Network.HTTP.Types (statusCode)
import Network.Wai
import qualified Utils.Logger as Logger
import Utils.UUIDGenerator

logMiddleware :: Middleware
logMiddleware app originalRequest sendResponse = do
  startTime <- liftIO getCurrentTime
  transactionUUID <- liftIO generateUUID
  generateResult <- generateId
  (req, bodyChunks) <- cacheRequestBody originalRequest
  reqestParameters <- encodeParameters bodyChunks

  let query = encodeQuery (queryString req)
      ipAddressFromHeader = fmap decodeUtf8 (getClientIP req)
      userAgentFromHeader = fmap decodeUtf8 (lookup "User-Agent" (requestHeaders req))
      contentTypeFromHeader = fmap decodeUtf8 (lookup "Content-Type" (requestHeaders req))
      authHeader = fmap (stripBearer . decodeUtf8) (lookup "Authorization" (requestHeaders req))
        where
          stripBearer token = fromMaybe token (stripPrefix "Bearer " token)
      urlFromRequest = decodeUtf8 (rawPathInfo req)
      methodFromRequest = pack (show (requestMethod req))

  case generateResult of
    Left err -> do
      Logger.logError $
        "[TraceID: N/A] Failed to generate ID: " ++ show err
      app req sendResponse
    Right logId -> do
      Logger.logInfo $
        "[TraceID: "
          ++ show logId
          ++ "] Request: "
          ++ show (requestMethod req)
          ++ " "
          ++ show (rawPathInfo req)
          ++ ", Body: "
          ++ show reqestParameters
          ++ ", Query: "
          ++ show query

      app req $ \response -> do
        endTime <- liftIO getCurrentTime
        extractedResponseMessage <- extractResponseMessage response
        operatorIdResult <- getOperatorId urlFromRequest reqestParameters authHeader
        let maybeOperatorId = either (const Nothing) id operatorIdResult

        let auditLog =
              AuditLog
                { auditLogId = logId,
                  transactionId = transactionUUID,
                  operatorId = maybeOperatorId,
                  ipAddress = ipAddressFromHeader,
                  userAgent = userAgentFromHeader,
                  description = Nothing,
                  url = urlFromRequest,
                  contentType = contentTypeFromHeader,
                  method = methodFromRequest,
                  parameters = reqestParameters,
                  queryText = query,
                  responseStatusCode = Just (statusCode (responseStatus response)),
                  responseMessage = Just $ extractedResponseMessage,
                  requestStartedAt = startTime,
                  requestEndedAt = Just endTime
                }

        Logger.logInfo $ "[TraceID: " ++ show logId ++ "] Response: " ++ show (responseStatus response)

        saveResult <- save auditLog
        case saveResult of
          Left saveErr ->
            Logger.logError $
              "[TraceID: " ++ show logId ++ "] Failed to save audit log: " ++ show saveErr
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
maskSensitiveData (Object obj) = object $ map maskKey (KM.toList obj)
  where
    maskKey :: (Key, Value) -> (Key, Value)
    maskKey (k, v)
      | toText k `elem` sensitiveKeys = (k, String "*****")
      | otherwise = (k, maskSensitiveData v)
maskSensitiveData (Array arr) = Array (fmap maskSensitiveData arr)
maskSensitiveData other = other

getClientIP :: Request -> Maybe BS.ByteString
getClientIP req =
  lookup "X-Forwarded-For" (requestHeaders req) <|> lookup "X-Real-IP" (requestHeaders req)

getOperatorId :: Text -> Maybe Text -> Maybe Text -> IO (Either SomeException (Maybe Int))
getOperatorId path reqestParameters accssTkn =
  let normalizedPath = dropApiVersionPrefix path
   in case () of
        _
          | "/auth/login" `isPrefixOf` normalizedPath ->
              case reqestParameters >>= extractFromParameters "userName" of
                Just userName -> getOperatorIdByUserName userName
                Nothing -> pure $ Right Nothing
          | "/auth/refresh" `isPrefixOf` normalizedPath ->
              case reqestParameters >>= extractFromParameters "refreshToken" of
                Just refreshToken -> getOperatorIdByRefreshToken refreshToken
                Nothing -> pure $ Right Nothing
          | otherwise ->
              case accssTkn of
                Just accessToken -> getOperatorIdByAccessToken accessToken
                Nothing -> pure $ Right Nothing

dropApiVersionPrefix :: Text -> Text
dropApiVersionPrefix path =
  case stripPrefix "/" path of
    Just stripped ->
      case splitOn "/" stripped of
        (_ : rest) -> "/" <> intercalate "/" rest
        _ -> path
    Nothing -> path

extractFromParameters :: Text -> Text -> Maybe Text
extractFromParameters key params =
  let decoded = decode (BSL.fromStrict $ encodeUtf8 params) :: Maybe Value
   in case decoded of
        Just (Object obj) ->
          case KM.lookup (fromText key) obj of
            Just (String t) -> Just t
            _ -> Nothing
        _ -> Nothing

extractResponseMessage :: Response -> IO Text
extractResponseMessage response = do
  let code = statusCode $ responseStatus response
  if code >= 400
    then do
      body <- getResponseBody response
      return $ pack $ unpack (decodeUtf8 (BSL.toStrict body))
    else return $ pack "Success"

getResponseBody :: Response -> IO ByteString
getResponseBody res =
  let (_, _, body) = responseToStream res
   in body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content
