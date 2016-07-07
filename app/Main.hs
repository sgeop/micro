module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

import Server

app :: Application
app = serve userApi server

main :: IO ()
main = run 8081 app
