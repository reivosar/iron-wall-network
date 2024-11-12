{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Application.Api (BankApi)
import Application.BankAccount.Server (bankAccountServer)

app :: Application
app = serve (Proxy :: Proxy BankApi) bankAccountServer

main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    run 8080 app
