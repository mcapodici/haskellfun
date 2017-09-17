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
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    description String
    done Bool
    deriving Show
|]

data WebApp = WebApp ConnectionPool

mkYesod "WebApp" [parseRoutes|
/ HomeR GET
|]

instance Yesod WebApp

instance YesodPersist WebApp where
    type YesodPersistBackend WebApp = SqlBackend

    runDB action = do
        WebApp pool <- getYesod
        runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
    tasks <- runDB $ selectList [] []
    defaultLayout $ do
        toWidget [lucius| .done { text-decoration: line-through ;} |]
        [whamlet|
            <ul>
                $forall Entity taskid task <- tasks
                    <li>
                        <span :taskDone task:.done>#{taskDescription task}
        |]
openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        deleteWhere ([] :: [Filter Task])
        insert $ Task "Buy Laundry Powder" False
        insert $ Task "Walk the dog" True
    warp 3000 $ WebApp pool
