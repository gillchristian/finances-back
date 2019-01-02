{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Pool
import Data.Int()
import Data.Dates
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
      :<|> "entries" :> QueryParam "month" Int
                     :> QueryParam "year" Int
                     :> Get '[JSON] [Entry]

-- DateTime	 
--   year :: Int
--   month :: Int
--   day :: Int
--   hour :: Int
--   minute :: Int
--   second :: Int

entryDate :: Int -> Int -> DateTime
entryDate y m = DateTime y m 0 0 0 0

srv :: Pool SqlBackend -> Server API
srv dbPool = newEntry :<|> entries
  where withDb f = liftIO $ runSqlPersistMPool f dbPool

        -- TODO: send the created entry
        newEntry :: Entry -> Handler Entry
        newEntry entry = do
          e <- withDb $ insertUnique entry
          case e of
            Nothing -> throwError err500
            Just _ -> return entry
          

        -- TODO: filter by from & to
        entries :: Maybe Int -> Maybe Int -> Handler [Entry]
        entries month year = do
          -- TODO: improve query
          -- select * from entries where
          --   from (month,year) > current (month,year) - 3
          --   AND
          --   from (month,year) < current (month,year) + 12
          es <- withDb $ selectList 
                          ( [EntryUntilMonth >. month, EntryUntilYear ==. year]
                            [EntryUntilMonth ==. month, EntryUntilYear ==. year]
                          ) 
                          []
          people <- selectList
            (     [PersonAge >. 25, PersonAge <=. 30]
              ||. [PersonFirstName /<-. ["Adam", "Bonny"]]
              ||. ([PersonAge ==. 50] ||. [PersonAge ==. 60])
            )
            []
          es <- withDb $ selectList [EntryUntilMonth ==. month, EntryUntilYear ==. year] []
          return $ map entityVal es

main :: IO ()
main = do
  -- Create the database
  Sqlite.runSqlite "example.db" $ Sqlite.runMigration migrateAll
  -- Initialize the connection pool
  runNoLoggingT $ Sqlite.withSqlitePool "example.db" 10 $ \dbPool ->
    NoLoggingT $ run 8080 $ serve (Proxy :: Proxy API) (srv dbPool)
      
