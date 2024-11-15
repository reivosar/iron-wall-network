{-# LANGUAGE OverloadedStrings #-}

module Utils.Logger (logInfo, logError) where

import System.Log.FastLogger
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO) 

{-# NOINLINE loggerSet #-}
loggerSet :: LoggerSet
loggerSet = unsafePerformIO $ newFileLoggerSet defaultBufSize "logfile.log"

-- Log INFO messages
logInfo :: String -> IO ()
logInfo msg = pushLogStrLn loggerSet (toLogStr ("INFO: " ++ msg))

-- Log ERROR messages
logError :: String -> IO ()
logError msg = pushLogStrLn loggerSet (toLogStr ("ERROR: " ++ msg))
