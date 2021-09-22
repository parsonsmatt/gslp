{-# language QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module APRE.DB where

import Data.Proxy
import Database.Persist.Sql
import Database.Persist.TH
import Lib
import Data.Time

mkPersist sqlSettings [persistLowerCase|

ApreSession
    lift                String
    date                Day
    whichRm             Int
    setOneReps          Reps
    setOneWeight        Weight
    setTwoReps          Reps
    setTwoWeight        Weight

|]

migrate :: SqlPersistT IO ()
migrate = runMigrationUnsafe $ migrateModels [entityDef (Proxy @ApreSession)]
