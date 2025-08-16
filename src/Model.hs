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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Database.Persist (Entity (Entity), SelectOpt (..), selectList, (<.), (<=.), (==.))
import Database.Persist.Sqlite (SqlBackend, fromSqlKey, (>=.))
import Database.Persist.TH (derivePersistField, mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

data Priority = High | Medium | Low
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Priority

instance FromJSON Priority

derivePersistField "Priority"

-- Database model definition
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Todo
 title T.Text
 completed Bool
 createdAt UTCTime
 updatedAt UTCTime
 dueDate UTCTime Maybe
 priority Priority
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
        "updatedAt" .= todoUpdatedAt todo,
        "dueDate" .= todoDueDate todo
      ]

-- Payload for receiving data from clients
data CreateTodoPayload = CreateTodoPayload
  { createTitle :: T.Text,
    createCompleted :: Maybe Bool,
    createDueDate :: Maybe UTCTime,
    createPriority :: Maybe Priority
  }
  deriving (Show, Generic, FromJSON, ToJSON)

validateDueDate :: Maybe UTCTime -> IO (Either String (Maybe UTCTime))
validateDueDate Nothing = return $ Right Nothing
validateDueDate (Just dueDate) = do
  now <- getCurrentTime
  return $
    if dueDate > now
      then Right (Just dueDate)
      else Left "Due date cannot be in the past"

getOverdueTodos :: (MonadIO m) => ReaderT SqlBackend m [Entity Todo]
getOverdueTodos = do
  now <- liftIO getCurrentTime
  selectList [TodoDueDate <. Just now, TodoCompleted ==. False] []

getTodosDueSoon :: (MonadIO m) => NominalDiffTime -> ReaderT SqlBackend m [Entity Todo]
getTodosDueSoon threshold = do
  now <- liftIO getCurrentTime
  let soonTime = addUTCTime threshold now
  selectList [TodoDueDate >=. Just now, TodoDueDate <=. Just soonTime, TodoCompleted ==. False] []

data UpdateTodoPayload = UpdateTodoPayload
  { updateTitle :: Maybe T.Text,
    updateCompleted :: Maybe Bool,
    updateDueDate :: Maybe UTCTime,
    updatePriority :: Maybe Priority
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
    ("duedate", Ascending) -> [Asc TodoDueDate]
    ("duedate", Descending) -> [Desc TodoDueDate]
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
