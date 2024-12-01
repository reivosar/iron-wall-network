{-# LANGUAGE DataKinds #-}

module Main (main) where

import Application.Api (BankApi)
import Application.BankAccount.Server (bankAccountServer)
import Network.Wai.Handler.Warp (run)
import Servant

app :: Application
app = serve (Proxy :: Proxy BankApi) bankAccountServer

main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  run 8080 app
