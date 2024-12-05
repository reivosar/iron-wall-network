{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Application.Auth.Api (AuthApi)
import Application.Auth.Server (authServer)
import Application.BankAccount.Api (BankApi)
import Application.BankAccount.Server (bankAccountServer)
import Middleware.EntryPointMiddleware (entryPointMiddleware)
import Network.Wai.Handler.Warp (run)
import Servant

type CombinedApi = BankApi :<|> AuthApi

app :: Application
app = entryPointMiddleware $ serve (Proxy :: Proxy CombinedApi) (bankAccountServer :<|> authServer)

main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  run 8080 app
