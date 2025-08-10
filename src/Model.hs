{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (Entity (Entity), SelectOpt (..))
import Database.Persist.Sqlite (fromSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

-- Database model definition
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Todo
 title T.Text
 completed Bool
 createdAt UTCTime
 updatedAt UTCTime
 deriving Show Eq Generic
|]

-- JSON instance for sending data to clients
instance ToJSON (Entity Todo) where
  toJSON (Entity todoId todo) =
    object
      [ "id" .= fromSqlKey todoId,
        "title" .= todoTitle todo,
        "completed" .= todoCompleted todo,
        "createdAt" .= todoCreatedAt todo,
        "updatedAt" .= todoUpdatedAt todo
      ]

-- Payload for receiving data from clients
data CreateTodoPayload = CreateTodoPayload
  { createTitle :: T.Text,
    createCompleted :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data UpdateTodoPayload = UpdateTodoPayload
  { updateTitle :: Maybe T.Text,
    updateCompleted :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- Response type with pagination metadata
data TodoResponse
  = TodoResponse
  { todos :: [Entity Todo],
    totalCount :: Int,
    limit :: Int,
    offset :: Int
  }
  deriving
    (Show, Generic, ToJSON)

data SortOrder = Ascending | Descending
  deriving (Show, Read, Eq)

data SortBy = SortBy T.Text SortOrder
  deriving (Show, Eq)

toSelectOpt :: Maybe SortBy -> [SelectOpt Todo]
toSelectOpt Nothing = [Desc TodoCreatedAt]
toSelectOpt (Just (SortBy field order)) =
  case (T.toLower field, order) of
    ("title", Ascending) -> [Asc TodoTitle]
    ("title", Descending) -> [Desc TodoTitle]
    ("completed", Ascending) -> [Asc TodoCompleted]
    ("completed", Descending) -> [Desc TodoCompleted]
    ("createdat", Ascending) -> [Asc TodoCreatedAt]
    ("createdat", Descending) -> [Desc TodoCreatedAt]
    ("updatedat", Ascending) -> [Asc TodoUpdatedAt]
    ("updatedat", Descending) -> [Desc TodoUpdatedAt]
    _ -> [Desc TodoCreatedAt]

-- Helper to parse sort parameters
parseOrder :: String -> Maybe SortOrder
parseOrder s = case map toLower s of
  "asc" -> Just Ascending
  "desc" -> Just Descending
  _ -> Nothing

parseSortParam :: Maybe String -> Maybe SortBy
parseSortParam Nothing = Nothing
parseSortParam (Just s) = case break (== ':') s of
  (field, ':' : order) -> SortBy (T.pack field) <$> parseOrder order
  (field, "") -> Just $ SortBy (T.pack field) Ascending
  _ -> Nothing
