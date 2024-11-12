{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database.DbUtils where

import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Control.Exception (try, SomeException, toException)
import Prelude (IO, Either(..), String, userError, pure)

connectDb :: IO (Either SomeException Connection)
connectDb = do
    dbNameResult <- try (getEnv "BACKEND_DB_NAME") :: IO (Either SomeException String)
    userResult <- try (getEnv "BACKEND_DB_USER") :: IO (Either SomeException String)
    passwordResult <- try (getEnv "BACKEND_DB_PASSWORD") :: IO (Either SomeException String)
    hostResult <- try (getEnv "BACKEND_DB_HOST") :: IO (Either SomeException String) 

    case (dbNameResult, userResult, passwordResult, hostResult) of
        (Right dbName, Right user, Right password, Right host) -> do
            conn <- connect defaultConnectInfo
                { connectDatabase = dbName
                , connectUser = user
                , connectPassword = password
                , connectHost = host
                }
            pure (Right conn)
        _ -> pure (Left (toException (userError "Failed to read environment variables")))
