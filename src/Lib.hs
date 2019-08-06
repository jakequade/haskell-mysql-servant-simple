-- Pulled from https://github.com/algas/haskell-servant-cookbook/blob/master/persistent/Main.hs
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Lib where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson
import           Data.Int                     (Int64 (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Database.Persist
import           Database.Persist.MySQL       (ConnectInfo (..),
                                               SqlBackend (..),
                                               defaultConnectInfo, fromSqlKey, runMigration,
                                               runSqlPool, toSqlKey, withMySQLConn)
import           Database.Persist.Sql         (SqlPersistT, runSqlConn)

import           Database.Persist.Types       (PersistValue(PersistInt64))

import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           System.Environment           (getArgs)
import qualified Data.UUID as U (UUID (..))
import Data.UUID.V4 (nextRandom)

import Controller.DealController (createDeal, deleteDeal, selectDealById, selectDeals)
import Db (doMigration)
import Model.Deal
import Model.ApiResponse

type MainAPI =
       "deals" :> Get '[JSON] [Deal]
  :<|> "deals" :> Capture "id" [Char] :> Get '[JSON] Deal
  :<|> "deals" :> Capture "id" [Char] :> Delete '[JSON] ()
  :<|> "deals" :> ReqBody '[JSON] Deal :> Post '[JSON] Deal

helloApi :: Proxy MainAPI
helloApi = Proxy

app :: Application
app = serve helloApi server

server :: Server MainAPI
server = 
  dealGET :<|> 
  dealGETById :<|> 
  dealDELETE :<|>
  dealPOST
    where
        dealGET = selectDeals
        dealGETById id = selectDealById id
        dealDELETE id = deleteDeal id
        dealPOST dealJson = createDeal dealJson

startApp :: IO ()
startApp = do
    args <- getArgs
    let arg1 = if not (null args) then Just (head args) else Nothing
    case arg1 of
        Just "migrate" -> doMigration
        _              -> run 8080 app