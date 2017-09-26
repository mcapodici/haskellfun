{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Foundation where

import Yesod
import Database.Persist.Sqlite

data WebApp = WebApp ConnectionPool

mkYesodData "WebApp" [parseRoutes|
/ HomeR GET
|]

instance Yesod WebApp

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    description String
    done Bool
    deriving Show
|]
instance YesodPersist WebApp where
    type YesodPersistBackend WebApp = SqlBackend

    runDB action = do
        WebApp pool <- getYesod
        runSqlPool action pool
