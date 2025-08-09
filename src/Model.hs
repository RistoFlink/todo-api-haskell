{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
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

module Model (
    -- Export the Todo type, its fields (..), and its migration
    module Model,
    TodoPayload(..)
) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject)
import Data.Text (Text)
import Database.Persist (Entity(Entity))
import Database.Persist.Sqlite (fromSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

-- Database model definition
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title Text
    completed Bool
    deriving Show Eq Generic
|]

-- JSON instance for sending data to clients
instance ToJSON (Entity Todo) where
  toJSON (Entity todoId todo) =
    object
      [ "id" .= fromSqlKey todoId
      , "title" .= todoTitle todo
      , "completed" .= todoCompleted todo
      ]

-- Payload for receiving data from clients
data TodoPayload = TodoPayload
  { payloadTitle :: Text
  , payloadCompleted :: Bool
  } deriving (Show, Generic)

instance FromJSON TodoPayload where
  parseJSON = withObject "TodoPayload" $ \o -> TodoPayload
    <$> o .: "title"
    <*> o .: "completed"

instance ToJSON TodoPayload where
  toJSON (TodoPayload title completed) = object
    [ "title" .= title
    , "completed" .= completed
    ]