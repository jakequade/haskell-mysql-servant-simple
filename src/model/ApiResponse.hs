{-# LANGUAGE DeriveGeneric              #-}
module Model.ApiResponse (ApiResponse(..), Error(..)) where

import Data.Aeson (ToJSON)
import GHC.Generics
import Model.Deal (Deal)

data Error = Error404 | Error503 deriving (Generic, Show)

data ApiResponse =
    ApiResponseSuccessful Deal
  | ApiResponseUnsucessful Error
  deriving (Generic, Show)

instance ToJSON ApiResponse
instance ToJSON Error