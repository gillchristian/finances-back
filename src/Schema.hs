{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses,
             EmptyDataDecls, FlexibleContexts, FlexibleInstances,
             GADTs, GeneralizedNewtypeDeriving #-}
module Schema where

import Database.Persist.TH

{-
recurring entry
  id                   id
  detail               text
  isPercentage         boolean
  isDebit              boolean
  from  (month,year)   (int,int)
  until (month,year)   Maybe (int,int)
-}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Entry json
  detail String
  amount Int
  isPercentage Bool
  isDebit Bool
  fromMonth Int
  fromYear Int
  untilMonth Int Maybe
  untilYear Int Maybe
  deriving Show
|]

