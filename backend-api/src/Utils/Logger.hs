{-# LANGUAGE OverloadedStrings #-}

module Utils.Logger (logInfo, logError) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Log.FastLogger

{-# NOINLINE loggerSet #-}
loggerSet :: LoggerSet
loggerSet = unsafePerformIO $ newFileLoggerSet defaultBufSize "logfile.log"

-- Get formatted current timestamp
getCurrentTimestamp :: IO String
getCurrentTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime

-- Log INFO messages
logInfo :: String -> IO ()
logInfo msg = do
  timestamp <- getCurrentTimestamp
  pushLogStrLn loggerSet (toLogStr ("INFO: [" ++ timestamp ++ "] " ++ msg))

-- Log ERROR messages
logError :: String -> IO ()
logError msg = do
  timestamp <- getCurrentTimestamp
  pushLogStrLn loggerSet (toLogStr ("ERROR: [" ++ timestamp ++ "] " ++ msg))
