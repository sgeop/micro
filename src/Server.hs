{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (catMaybes)
import Database.Persist.Sqlite
import Data.Aeson.Types
import GHC.Generics
import Servant

import Models


data PostSuccess = PostSuccess { userId :: Int }
  deriving (Generic, Show)

instance ToJSON PostSuccess


type UserGet = "user"
  :> QueryParam "first_name" String
  :> QueryParam "last_name" String
  :> QueryParam "age" Int
  :> QueryParam "email" String
  :> Get '[JSON] [User]

type UserPost = "user" :> ReqBody '[JSON] User :> Post '[JSON] PostSuccess

type UserApi = UserGet :<|> UserPost


server :: Server UserApi
server = getHandler :<|> postHandler

  where getHandler
          :: Maybe String
          -> Maybe String
          -> Maybe Int
          -> Maybe String
          -> Handler [User]
        getHandler fname lname age email = do
          res <- runSqlite "sqlite.db" $ selectList
            (catMaybes
              [ fmap (UserFirstName ==.) fname
              , fmap (UserLastName ==.) lname
              , fmap (UserAge ==.) age
              , fmap (UserEmail ==.) email
              ]) []
          return $ map entityVal res

        postHandler :: User -> Handler PostSuccess
        postHandler user = do
          res <- runSqlite "sqlite.db" $ insert user
          return PostSuccess { userId = fromIntegral $ fromSqlKey res }


userApi :: Proxy UserApi
userApi = Proxy
