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
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Database.Persist (Entity (Entity))
import Database.Persist.Sqlite (fromSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

-- Database model definition
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Todo
 title Text
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
data TodoPayload = TodoPayload
  { title :: Text,
    completed :: Bool
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

data SortOrder = Asc | Desc
  deriving (Show, Read, Eq)

data SortBy = SortBy Text SortOrder
  deriving (Show, Eq)

-- Helper to parse sort parameters
parseOrder :: String -> Maybe SortOrder
parseOrder s = case map toLower s of
  "asc" -> Just Asc
  "desc" -> Just Desc
  _ -> Nothing

parseSortParam :: Maybe String -> Maybe SortBy
parseSortParam Nothing = Nothing
parseSortParam (Just s) = case break (== ':') s of
  (field, ':' : order) -> SortBy (pack field) <$> parseOrder order
  (field, "") -> Just $ SortBy (pack field) Asc
  _ -> Nothing
