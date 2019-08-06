-- TODO: remove what isn't needed
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

module Model.Deal (Deal(..), Key(DealKey), migrateAll) where

import           Database.Persist.TH          (mkMigrate, mkPersist,
                                               persistLowerCase, share,
                                               sqlSettings)
import Data.Text
import Data.UUID
import Database.Persist
import Database.Persist.Sql (PersistFieldSql (..))
import GHC.Generics

import Model.DealId

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Deal json
    Id          UUID Primary Unique sqltype=varchar(50)
    title       Text
    description Text
    deriving Eq Show Generic
|]
