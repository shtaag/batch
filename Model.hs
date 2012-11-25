{-# LANGUAGE TypeFamilies, TemplateHaskell, FlexibleContexts, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
module Model where

import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name Text
    age Int
    sex Sex
  deriving Show

NumPerAge
    ageArea Int
    sex Sex
    number Int
  deriving Show
|]

data Sex = Male | Female
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Sex"