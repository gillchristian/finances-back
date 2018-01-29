{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Pool
import Database.Persist
import Database.Persist.Sql
import qualified Database.Persist.Sqlite as Sqlite
import Network.Wai.Handler.Warp
import Servant
-- TODO: add validation
-- import Data.Validator

import Schema

type API = "entries" :> ReqBody '[JSON] Entry
                     :> Post '[JSON] Entry
      :<|> "entries" :> QueryParam "from" Integer
                     :> QueryParam "to" Integer
                     :> Get '[JSON] [Entry]

srv :: Pool SqlBackend -> Server API
srv dbPool = newEntry :<|> entries
  where withDb f = liftIO $ runSqlPersistMPool f dbPool

        -- TODO: send the created entry
        newEntry :: Entry -> Handler Entry
        newEntry entry = do
          e <- withDb $ insertUnique entry
          case e of
            Nothing -> throwError err500
            Just e' -> return entry
          

        -- TODO: filter by from & to
        entries :: Maybe Integer -> Maybe Integer -> Handler [Entry]
        entries from to =  do
          es <- withDb $ selectList [] []
          return $ map entityVal es


main :: IO ()
main = do
  -- Create the database
  Sqlite.runSqlite "example.db" $ Sqlite.runMigration migrateAll
  -- Initialize the connection pool
  runNoLoggingT $ Sqlite.withSqlitePool "example.db" 10 $ \dbPool ->
    NoLoggingT $ run 8080 $ serve (Proxy :: Proxy API) (srv dbPool)
      
