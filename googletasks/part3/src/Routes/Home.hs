{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module Routes.Home where

import Yesod
import Database.Persist.Sqlite
import Foundation

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
