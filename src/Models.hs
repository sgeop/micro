{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Aeson
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  firstName String
  lastName String
  age Int
  email String
  deriving Show
|]


type SqlResult a = forall (m :: * -> *). MonadIO m => SqlPersistT m a

setupDB :: SqlResult ()
setupDB = do
  runMigration migrateAll
  insertMany
    [ User "Joe" "Schmoe" 25 "joeschmoe99@gmail.com"
    , User "Jeff" "Brown" 12 "jb12345@yahoo.com"
    ]
  return ()
