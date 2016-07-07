{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html


type UserApi = "users" :> Get '[JSON] [User]
          :<|> "user" :> Capture "uid" Int :> Get '[JSON] User


data User = User
  { userId :: Int
  , firstName :: String
  , lastName :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User


userList =
  [ User 1 "Joe" "Schmoe" 25 "joeschmoe99@gmail.com"
  , User 2 "Jane" "Jacobs" 32 "plain_jain12345@yahoo.com"
  ]


server :: Server UserApi
server = users :<|> user

  where users :: Handler [User]
        users = return userList

        user :: Int -> Handler User
        user uid = return (head $ filter (\u -> userId u == uid) userList)


userApi :: Proxy UserApi
userApi = Proxy
