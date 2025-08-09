{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad (
    AppM(..),
    AppConfig(..),
    runDb
) where

-- Necessary imports for the types used
import Servant (Handler, ServerError)
import Control.Monad.Reader (ReaderT, asks, MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.Sqlite (ConnectionPool, runSqlPool)

-- The configuration record for the application
newtype AppConfig = AppConfig
    { appPool :: ConnectionPool
    }

-- The custom monad for the application
-- It stacks the ReaderT transformer on top of Servant's Handler.
newtype AppM a = AppM (ReaderT AppConfig Handler a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadError ServerError)

-- Helper to run database queries within the AppM monad.
runDb :: SqlPersistT IO a -> AppM a
runDb query = do
    pool <- asks appPool
    liftIO $ runSqlPool query pool