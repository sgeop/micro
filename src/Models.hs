{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
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
import Data.Text
import GHC.Generics (Generic)
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  firstName String
  lastName String
  age Int
  email String
  deriving Show
|]

run :: IO ()
run = runSqlite ":memory" $ do
  runMigration migrateAll
  deleteWhere [UserFirstName ==. "Joe"]
  insert $ User "Joe" "Schmoe" 25 "joeschmoe99@gmail.com"
  insert $ User "Jeff" "Brown" 12 "jb12345@yahoo.com"
  adults <- selectList [UserAge >. 18, UserAge <. 30] [LimitTo 2]
  liftIO $ print adults
