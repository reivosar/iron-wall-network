{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database.DbUtils where

import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Control.Exception (try, SomeException)

connectDb :: IO (Either SomeException Connection)
connectDb = do
    dbNameResult <- try (getEnv "BACKEND_DB_NAME") :: IO (Either SomeException String)
    userResult <- try (getEnv "BACKEND_DB_USER") :: IO (Either SomeException String)
    passwordResult <- try (getEnv "BACKEND_DB_PASSWORD") :: IO (Either SomeException String)

    case (dbNameResult, userResult, passwordResult) of
        (Right dbName, Right user, Right password) -> do
            conn <- connect defaultConnectInfo
                { connectDatabase = dbName
                , connectUser = user
                , connectPassword = password
                }
            return (Right conn)
        _ -> return (Left (toException (userError "Failed to read environment variables")))

