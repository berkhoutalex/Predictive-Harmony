{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Types (Scientist, scientists)

type API = "scientists" :> Get '[JSON] [Scientist]

startApp :: IO ()
startApp = do
  putStrLn "Starting"
  run 8001 app
  putStrLn "Finished"

app :: Application
app = simpleCors (serve api server)

api :: Proxy API
api = Proxy

server :: Server API
server = return scientists
