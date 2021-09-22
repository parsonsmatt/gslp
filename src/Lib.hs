{-# LANGUAGE TypeApplications, GADTs, FlexibleInstances, OverloadedLists #-}

module Lib where

import Control.Monad (join)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (Day, fromGregorian)
import qualified Data.List as List
import Database.Persist.Sql

data Lift = Lift { liftName :: String, liftSets :: NonEmpty Set }

(%:) :: String -> NonEmpty Set -> Lift
name %: lifts = Lift name lifts

(#:) :: Day -> NonEmpty Lift -> Session
(#:) = flip Session

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

instance (a ~ Int, b ~ Int) => Num (a -> b -> Day) where
    fromInteger y m d = fromGregorian y m d
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    negate = undefined

foo :: Set
foo = 60 x 5

newtype Reps = Reps { unReps :: Int }
    deriving newtype (Eq, Ord, Show, PersistField, PersistFieldSql)

newtype Weight = Weight { weightAmount :: Double }
    deriving newtype (Eq, Show, Ord, Num, Real, Fractional, RealFrac, PersistField, PersistFieldSql)

data Set = Set
    { setWeight :: Weight
    , setReps :: Reps
    }
    deriving Show

data Session = Session
    { sessionLifts :: NonEmpty Lift
    , sessionDate :: Day
    -- ^ If the date is 'Nothing', then this is a planned workout and not
    -- one that has actually occurred yet.
    }

data Entry = Entry Day (Either Bodyweight Session)

type History = NonEmpty Entry

newtype Bodyweight = Bodyweight { unBodyweight :: Weight }

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

dateLifts :: NonEmpty Session -> Map Day (NonEmpty Lift)
dateLifts = foldr (\s -> Map.insert (sessionDate s) (sessionLifts s)) Map.empty

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
    . List.sortOn e1rm
    . NEL.toList

--
findLast :: String -> NonEmpty Session -> Maybe Session
findLast lift = id
    . fmap (\(a, b) -> Session b a)
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

squat, press, row, deadlift, curl, reverseHyper, latPull :: String
squat = "Squat"
press = "Press"
row = "Row"
deadlift = "Deadlift"
curl = "Curl"
reverseHyper = "Reverse Hyper"
latPull = "Lat Pull"

roundTo :: RealFrac a => a -> a -> a
roundTo increment amount =
    increment * fromInteger (floor (amount / increment))

pf :: String -> String -> String
pf i a = (i ++ " " ++ a)

bench, db, romanian :: String -> String
bench = pf "Bench "
db = pf "DB "
romanian = pf "Romanian "

e1rm :: Set -> Double
e1rm (Set (Weight w) (Reps i)) =
    (100 * w) / (101.3 - (2.67123 * fromIntegral i))
