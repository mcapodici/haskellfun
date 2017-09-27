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
import Yesod.Static (Static, staticFiles, static)

data WebApp = WebApp
  { getConnectionPool :: ConnectionPool
  , getStatic :: Static
  }

staticFiles "static"

mkYesodData "WebApp" [parseRoutes|
/ HomeR GET
/static StaticR Static getStatic
|]

instance Yesod WebApp where
  shouldLog (WebApp _ _) src level = True
  defaultLayout contents = do
      PageContent title headTags bodyTags <- widgetToPageContent contents
      mmsg <- getMessage
      withUrlRenderer [hamlet|
          $doctype 5

          <html>
              <head>
                  <title>#{title}
                  <link rel="stylesheet" type="text/css" href="@{StaticR simple_grid_min_css}">
                  <link rel="stylesheet" type="text/css" href="@{StaticR style_css}">
                  ^{headTags}
                  
              <body>
                  ^{bodyTags}
      |]
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    description String
    done Bool
    deriving Show
|]
instance YesodPersist WebApp where
    type YesodPersistBackend WebApp = SqlBackend

    runDB action = do
        webApp <- getYesod
        runSqlPool action $ getConnectionPool webApp

