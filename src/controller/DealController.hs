module Controller.DealController where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson as JSON
import Database.Persist (Entity(..), delete, get, insert, selectList)
import Db (runDB)
import Model.ApiResponse (ApiResponse(..), Error(..))
import Model.Deal (Deal (..), Key(DealKey))
import qualified Data.UUID as U (UUID(..), fromString, toString)
import Servant

selectDeals :: Handler [Deal]
selectDeals = do
  dealList <- runDB $ selectList [] []
  return $ map (\(Entity _ u) -> u) dealList

selectDealById :: [Char] -> Handler Deal
selectDealById uuidString = case U.fromString uuidString of
  Just uuid -> do
    sqlResult <- runDB $ get $ DealKey $ uuid
    case sqlResult of
      Just deal -> return deal
      Nothing -> throwError err404 { errBody = JSON.encode "No deal with that id found." }
  Nothing -> throwError err403 { errBody = JSON.encode "ID could not be parsed." }

createDeal :: Deal -> Handler Deal
createDeal deal = do
  attemptCreate <- runDB $ insert $ deal
  case attemptCreate of
    DealKey k -> return deal
    _         -> throwError err503 { errBody = JSON.encode "Could not create deal." }

deleteDeal :: String -> Handler ()
deleteDeal uuidString = case U.fromString uuidString of
  Just uuid -> do runDB $ delete $ DealKey uuid
  -- Do nothing
  Nothing -> return ()