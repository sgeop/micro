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
import Data.Maybe (catMaybes)
import Database.Persist.Sqlite
import Servant

import Models


type UserApi = "user"
  :> QueryParam "first_name" String
  :> QueryParam "last_name" String
  :> QueryParam "age" Int
  :> QueryParam "email" String
  :> Get '[JSON] [User]


userHandler
  :: Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe String
  -> Handler [User]
userHandler fname lname age email = do
  res <- runSqlite "sqlite.db" $ selectList
    (catMaybes
      [ fmap (UserFirstName ==.) fname
      , fmap (UserLastName ==.) lname
      , fmap (UserAge ==.) age
      , fmap (UserEmail ==.) email
      ]) []
  return $ map entityVal res


server :: Server UserApi
server = userHandler


userApi :: Proxy UserApi
userApi = Proxy
