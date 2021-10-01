{-# language QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# language OverloadedLabels #-}

module APRE.DB where

import Data.Foldable
import Data.Proxy
import Database.Persist.Sql
import Database.Persist.TH
import Lib
import Data.Time
import qualified Data.Set as Set

mkPersist sqlSettings [persistLowerCase|

ApreSession
    exerciseId          ExerciseId
    date                Day
    whichRm             Int
    setOneReps          Reps
    setOneWeight        Weight
    setTwoReps          Reps
    setTwoWeight        Weight

Exercise
    name        String
    increment   Weight

    UniqueExerciseName name

|]

migrate :: SqlPersistT IO ()
migrate = do
    runMigrationUnsafe $ migrateModels
        [ entityDef (Proxy @Exercise)
        , entityDef (Proxy @ApreSession)
        ]
