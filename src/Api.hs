{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Database.Persist (Entity)
import Model
import Servant

type TodoAPI =
  "health" :> Get '[JSON] String
    :<|> "todos" :> QueryParam "completed" Bool :> Get '[JSON] [Entity Todo]
    :<|> "todos" :> ReqBody '[JSON] TodoPayload :> Post '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> Get '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON] TodoPayload :> Put '[JSON] (Entity Todo)
    :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] ()