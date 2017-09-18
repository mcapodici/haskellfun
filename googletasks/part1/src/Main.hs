{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
 
module Main where
 
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
 
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    description String
    done Bool
    deriving Show
|]
 
main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
 
    t1Id <- insert $ Task "Buy Laundry Powder" False
    t2Id <- insert $ Task "Walk the dog" True
 
    task1 <- get t1Id
    liftIO $ print task1
