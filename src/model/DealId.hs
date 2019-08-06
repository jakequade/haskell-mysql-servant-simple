module Model.DealId where

import qualified Data.Text as T (pack, unpack)
import Data.UUID (UUID(..), fromString, toString)
import Database.Persist (PersistField(..), PersistValue(PersistText), SqlType(SqlString))
import Database.Persist.Sql (PersistFieldSql(..))
import Web.PathPieces (PathPiece(..), readFromPathPiece, showToPathPiece)

instance PersistFieldSql UUID where
    sqlType _ = SqlString

instance PersistField UUID where
    toPersistValue u = PersistText $ T.pack $ toString u
    fromPersistValue s =
      case fromString (show s) of
        Just u -> Right u
        Nothing -> Left $ T.pack "Invalid UUID"
    fromPersistValue _ = Left $ T.pack "Not a valid persist value"

instance PathPiece UUID where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece