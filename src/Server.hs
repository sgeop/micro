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

type UserApi = "user"
  :> QueryParam "first_name" String
  :> QueryParam "last_name" String
  :> QueryParam "email" String
  :> Get '[JSON] [User]

userHandler
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Handler [User]
userHandler fn ln email = do
  u <- runSqlite "db" $ userSearch
    [ (UserFirstName, fn)
    , (UserLastName, ln)
    , (UserEmail, email)
    ]
  case u of
    [] -> throwError err503 { errBody = "email not found" }
    _ -> return u


server :: Server UserApi
server = userHandler

userApi :: Proxy UserApi
userApi = Proxy

