{-# LANGUAGE TemplateHaskell            #-} 
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Schema where

import Database.Persist.TH

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

