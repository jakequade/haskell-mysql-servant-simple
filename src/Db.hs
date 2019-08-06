module Db where

import Data.Aeson (encode)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.MySQL (ConnectInfo (..), SqlBackend (..),
                               defaultConnectInfo, fromSqlKey, runMigration,
                               runSqlPool, toSqlKey, withMySQLConn)
import Database.Persist.Sql (SqlPersistT, runSqlConn)
import Model.Deal
import Servant (Handler, errBody, throwError)

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> Handler a
runDB a = liftIO $ runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runSqlConn a

connInfo :: ConnectInfo
connInfo = defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "root", connectPassword = "abcd1234", connectDatabase = "deals-api" }

doMigration :: IO ()
doMigration = runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runReaderT $ runMigration migrateAll