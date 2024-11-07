{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

type API = Get '[PlainText] String 

server :: Server API
server = return "Hello, World!"

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    run 8080 app
