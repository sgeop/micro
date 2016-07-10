{-# LANGUAGE DataKinds #-}
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
import Data.Text
import Database.Persist.Sqlite (runSqlite)
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

import Models

type UserApi = "users" :> Get '[JSON] [User]
          :<|> "user" :> QueryParam "email" String :> Get '[JSON] [User]


server :: Server UserApi
server = users :<|> user

  where users :: Handler [User]
        users = runSqlite "db" allUsers

        user :: Maybe String -> Handler [User]
        user  Nothing = runSqlite "db" allUsers
        user  (Just email) = do
            u <- runSqlite "db" $ userByEmail email
            case u of
              [] -> throwError err503 { errBody = "email not found" }
              _ -> return u


userApi :: Proxy UserApi
userApi = Proxy
