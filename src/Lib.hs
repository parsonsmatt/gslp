{-# LANGUAGE TypeApplications, GADTs, FlexibleInstances, OverloadedLists #-}

module Lib where

import Control.Monad (join)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (Day, fromGregorian)
import qualified Data.List as List

data Lift = Lift { liftName :: String, liftSets :: NonEmpty Set }

(%:) :: String -> NonEmpty Set -> Lift
name %: lifts = Lift name lifts

(*:) :: Set -> Int -> NonEmpty Set
(*:) w i = w :| replicate (i-1) w

infixr 4 *:

infixr 3 %:


lift :: String -> NonEmpty Set -> Lift
lift = Lift

x :: ()
x = ()

instance (a ~ Int) => Num (() -> a -> Set) where
    fromInteger i =  \_ r -> Set (Weight (fromInteger i)) (Reps r)
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    negate = undefined

instance (a ~ Int, b ~ Int) => Num (() -> a -> () -> b -> NonEmpty Set) where
    fromInteger i = \() r () s -> fromInteger i x r *: s
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    negate = undefined

foo :: Set
foo = 60 x 5

newtype Reps = Reps { unReps :: Int }
    deriving (Eq, Ord)

newtype Weight = Weight { weightAmount :: Double }

data Set = Set
    { setWeight :: Weight
    , setReps :: Reps
    }

data Session = Session
    { sessionLifts :: NonEmpty Lift
    , sessionDate :: Maybe Day
    -- ^ If the date is 'Nothing', then this is a planned workout and not
    -- one that has actually occurred yet.
    }

newtype Bodyweight = Bodyweight { unBodyweight :: Weight }

type History = NonEmpty Session

type Program = NonEmpty Session -> Session

greyskullStep :: Set -> Set
greyskullStep (Set weight reps) =
    Set newWeight (Reps 5)
  where
    newWeight
        | Reps 10 >= reps =
            Weight (weightAmount weight + 10)
        | Reps 5 >= reps =
            Weight (weightAmount weight + 5)
        | otherwise =
            Weight
                . fromInteger . (* 5) . floor . (/ 5) . (* 0.9)
                . weightAmount $ weight

wilksFormula :: Bodyweight -> Weight -> Double
wilksFormula bw (Weight total) = (total *) $
    500
    /
    sum ([a, b * x, c * x ^ 2, d * x ^ 3, e * x ^ 4, f * x ^ 5] :: [Double])
  where
    a = -216.0475144
    b = 16.260339
    c = -0.002388645
    d = -0.00113732
    e = 7.01863E-06
    f = -1.291E-08
    Bodyweight (Weight x) = bw

firstSession :: Session
firstSession = Session
    { sessionLifts =
        [bench, squat, rows, dbPress]
    , sessionDate =
        Nothing
    }
  where
    barx5 = Set (Weight 45) (Reps 5)
    sets = barx5 :| replicate 2 barx5
    squat = Lift
        { liftName = "Squat"
        , liftSets = sets
        }
    bench = Lift
        { liftName = "Bench Press"
        , liftSets = sets
        }
    rows = Lift
        { liftName = "DB Row"
        , liftSets =
            let s = Set (Weight 45) (Reps 12)
             in s :| replicate 3 s
        }
    dbPress = Lift
        { liftName = "DB Press"
        , liftSets =
            let s = Set (Weight 25) (Reps 12)
             in s :| replicate 3 s
        }

secondSession :: Session
secondSession = Session
    { sessionLifts =
        press :| [deadlift, curls]
    , sessionDate =
        Nothing
    }
  where
    barx5 = Set (Weight 45) (Reps 5)
    sets = barx5 :| replicate 2 barx5
    deadlift = Lift
        { liftName = "Deadlift"
        , liftSets = sets
        }
    press = Lift
        { liftName = "Press"
        , liftSets = sets
        }
    curls = Lift
        { liftName = "DB Curl"
        , liftSets =
            let s = Set (Weight 25) (Reps 12)
             in s :| replicate 3 s
        }

history :: NonEmpty Session
history =
    [ Session
        { sessionDate = Just (fromGregorian 2019 03 17)
        , sessionLifts =
            [ squat %:
                135 x 5 x 3
            , bench press %:
                [ 115 x 5
                , 115 x 5
                , 115 x 12
                ]
            , db row %:
                60 x 12 x 4
            ]
        }
    , Session
        { sessionDate = Just (fromGregorian 2019 03 19)
        , sessionLifts =
            [ press %:
                65 x 5 x 2
                <>
                65 x 12 x 1
            , deadlift %:
                155 x 5 x 3
            , db curl %:
                30 x 12 x 4
            ]
        }
    ]

dateLifts :: NonEmpty Session -> Map Day (NonEmpty Lift)
dateLifts = foldr (\s -> case sessionDate s of
    Nothing -> id
    Just d -> Map.insert d (sessionLifts s)) Map.empty

-- | Property:
--
-- >>> all (all ((str ==) . liftName)) (onlyLiftsOf str)
-- True
onlyLiftsOf :: String -> Map Day (NonEmpty Lift) -> Map Day (NonEmpty Lift)
onlyLiftsOf str = Map.mapMaybe (NEL.nonEmpty . NEL.filter p) . Map.filter (any p)
  where
    p = (str ==) . liftName

bestSet :: NonEmpty Set -> Set
bestSet = id
    . head
    . List.sortOn (\set -> e1rm set)
    . NEL.toList

--
findLast :: String -> NonEmpty Session -> Maybe Session
findLast lift = id
    . fmap (\(a, b) -> Session b (Just a))
    . Map.lookupMax
    . onlyLiftsOf lift
    . dateLifts

currentWilks :: NonEmpty Session -> Bodyweight -> Maybe Double
currentWilks sess bw =
    wilksFormula <$> pure bw <*> liftSessions
  where
    liftSessions :: Maybe Weight
    liftSessions = case (,,) <$> msq <*> mdl <*> mbp of
        Nothing -> Nothing
        Just (sq, dl, bp) -> Just $
           Weight $ sum @[] [f sq, f dl, f bp]

    f :: Session -> Double
    f = weightAmount . setWeight . bestSet . join . fmap liftSets . sessionLifts

    msq = findLast squat sess
    mdl = findLast deadlift sess
    mbp = findLast (bench press) sess

squat, press, row, deadlift, curl :: String
squat = "Squat"
press = "Press"
row = "Row"
deadlift = "Deadlift"
curl = "Curl"

pf :: String -> String -> String
pf i a = (i ++ " " ++ a)

bench, db, romanian :: String -> String
bench = pf "Bench "
db = pf "DB "
romanian = pf "Romanian "

e1rm :: Set -> Double
e1rm (Set (Weight w) (Reps i)) = factor * w
  where
    factor = case i of
        1 -> 1
        2 -> 0.95
        3 -> 0.93
        4 -> 0.9
        5 -> 0.87
        6 -> 0.85
        7 -> 0.83
        8 -> 0.80
        9 -> 0.77
        10 -> 0.75
        11 -> 0.73
        12 -> 0.70
        _ -> 0.68
