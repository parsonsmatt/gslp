-- | APRE is a style of lifting where you go "to failure" (or to RPE 8-9),
-- and adjust reps from there.
module APRE where

import Prelude hiding (log)

import qualified Data.List as List
import Lib hiding (lift)
import Data.Time
import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified APRE.DB as DB
import System.IO
import Text.Read (readMaybe)
import Data.Foldable (for_)

todaysLifting :: IO ()
todaysLifting = do
    withDb $ \conn -> do
        hSetBuffering stdout NoBuffering
        let
            runDb
                :: SqlPersistT IO a -> IO a
            runDb q =
                runSqlConn q conn
        runDb DB.migrate

        log ["Welcome to today's APRE session. Fetching exercises . . ."]
        exercises <- runDb $ selectList [] []
        let options = zip [1 :: Int ..] exercises
        log ["Options: "]
        for_ options $ \(idx, Entity optionKey option) -> do
            mlastSession <-
                runDb $ selectFirst
                    [ DB.ApreSessionExerciseId ==. optionKey
                    ]
                    [ Desc DB.ApreSessionDate
                    ]
            let
                lastSessionDate =
                    fmap (DB.apreSessionDate . entityVal) mlastSession
                renderedDate =
                    case lastSessionDate of
                        Nothing ->
                            "(no sessions recorded)"
                        Just day ->
                            "(" <> show day <> ")"

            log ["\t", show idx, ". ", DB.exerciseName option, "\t", renderedDate]
        log ["What lift are you doing?"]
        log ["(select a number to choose an above lift, or input text to provide a new one)"]

        liftStr <-
            promptStr
                [ "(# or name): "
                ]

        exercise <-
            case readMaybe liftStr of
                Nothing ->
                    case List.find (\(_, Entity _ e) -> liftStr == DB.exerciseName e) options of
                        Nothing -> do
                            i <- Weight <$> prompt ["What increment do you want to use for this exercise?"]
                            runDb $ insertEntity DB.Exercise
                                { exerciseName = liftStr
                                , exerciseIncrement = i
                                }
                        Just (_, a) ->
                            pure a

                Just i ->
                    case List.lookup i options of
                        Nothing ->
                            error "Failed to find exericse of that number."
                        Just a ->
                            pure a

        let lift = DB.exerciseName (entityVal exercise)

        sessions <-
            runDb $
                selectList
                    [DB.ApreSessionExerciseId ==. entityKey exercise]
                    [Desc DB.ApreSessionDate, LimitTo 1]

        currentPlan <-
            case sessions of
                [] -> do
                    log ["No sessions found for <<", lift, ">>. Starting a new lifting history."]
                    whichRm <- prompt ["What target RM are you hitting? (3, 6, 10)"]
                    startWeight <- prompt ["What weight do you want to start with?"]
                    increment <- Weight <$> prompt ["What step increment do you want? (2.5 for upper, 5 for lower)"]
                    pure $ mkSessionPlan increment whichRm (Weight startWeight)
                (Entity _ prevSession : _) -> do
                    log ["Found a session from ", show (DB.apreSessionDate prevSession)]
                    let
                        prevReps =
                            DB.apreSessionSetTwoReps prevSession
                        prevWeight =
                            DB.apreSessionSetTwoWeight prevSession
                        whichRm =
                            DB.apreSessionWhichRm prevSession
                        increment =
                            DB.exerciseIncrement $ entityVal exercise
                    log
                        [ "You finished with "
                        , show prevReps
                        , " reps at "
                        , show prevWeight
                        , " pounds. Nice!"
                        ]
                    pure $ mkSessionPlan increment whichRm (nextMainSet increment whichRm prevWeight prevReps)

        apreSession <- runPlanInteractive currentPlan

        today <- localDay . zonedTimeToLocalTime <$> getZonedTime
        _ <- runDb $ insert $
            mkDbSessionFor exercise today apreSession

        log ["Great job!"]

  where
    mkDbSessionFor lift today ApreSession { plan = ApreSessionPlan {..}, ..} =
        DB.ApreSession
            { DB.apreSessionExerciseId =
                entityKey lift
            , DB.apreSessionDate =
                today
            , DB.apreSessionSetOneReps =
                setReps set1Reps
            , DB.apreSessionSetOneWeight =
                setWeight set1Reps
            , DB.apreSessionSetTwoReps =
                setReps set2Reps
            , DB.apreSessionSetTwoWeight =
                setWeight set2Reps
            , DB.apreSessionWhichRm =
                apreWhichRm
            }

    log strs =
        putStrLn $ concat strs
    prompt :: Read a => [String] -> IO a
    prompt msg = do
        ma <- promptStr msg
        case readMaybe ma of
            Nothing -> do
                log ["That didn't make sense to me - you entered: ", ma]
                prompt msg
            Just a ->
                pure a
    promptStr :: [String] -> IO String
    promptStr msg = do
        putStr $ concat msg ++ "\t"
        getLine


    withDb action =
        runNoLoggingT $ withSqliteConn "apre.sqlite3" $ \conn -> liftIO $ action conn

progressionGeneric
    :: (Ord a, Num a)
    => a
    -- ^ The weight increment to use per rep
    -> a
    -- ^ The target rep amount
    -> a
    -- ^ The number of reps accomplished
    -> a
    -- ^ The weight accomplished
    -> a
    -- ^ The weight for the next main set
progressionGeneric increment target repsAccomplished thisWeight =
    thisWeight + modification
  where
    modification =
        repDelta * increment
    repDelta =
        repsAccomplished - target

progression3rm :: (Ord a, Num a) => a -> a -> a
progression3rm reps
    | reps == 3 || reps == 4 =
        id
    | otherwise =
        (+ ((reps - 4) * 5))

progression6rm :: (Ord a, Num a) => a -> a -> a
progression6rm reps
    | reps >= 5 && reps <= 7 =
        id
    | otherwise =
        (+ ((reps - 7) * 5))

progression10rm :: (Ord a, Num a) => a -> a -> a
progression10rm reps
    | reps >= 9 && reps <= 11 =
        id
    | otherwise =
        (+ ((reps - 11) * 5))


data ApreSessionPlan = ApreSessionPlan
    { warmup1 :: Set
    , warmup2 :: Set
    , set1  :: Weight
    , apreWhichRm :: Int
    , apreIncrement :: Weight
    }
    deriving Show

apreSessionFinalSet :: ApreSessionPlan -> Reps -> Weight
apreSessionFinalSet apreSessionPlan reps =
    nextMainSet (apreIncrement apreSessionPlan) (apreWhichRm apreSessionPlan) (set1 apreSessionPlan) reps

nextMainSet :: Weight -> Int -> Weight -> Reps -> Weight
nextMainSet increment whichRm weight (Reps reps) =
    roundTo 2.5 $ progression (fromIntegral reps) weight
  where
    progression =
        progressionGeneric (increment) (fromIntegral whichRm)

data ApreSession = ApreSession
    { plan :: ApreSessionPlan
    , set1Reps :: Set
    , set2Reps :: Set
    }
    deriving Show

nextPlan :: ApreSession -> ApreSessionPlan
nextPlan apreSession =
    mkSessionPlan incr whichRm $
        nextMainSet incr whichRm (setWeight finalSet) (setReps finalSet)
  where
    incr = apreIncrement $ plan apreSession
    whichRm = apreWhichRm plan'
    plan' = plan apreSession
    finalSet = set2Reps apreSession

runPlanInteractive :: ApreSessionPlan -> IO ApreSession
runPlanInteractive apreSessionPlan = do
    putStrLn " ~ * ~ crush it king/queen/monarch ~ * ~"
    putStrLn "      << hit enter to go to next step >>"
    putStrLn $ "Warm up 1: \t\t" ++ show (warmup1 apreSessionPlan)
    _ <- getLine
    putStrLn $ "Warm up 2: \t\t" ++ show (warmup2 apreSessionPlan)
    _ <- getLine
    putStrLn $ "Set #1:  \t\t" ++ show (set1 apreSessionPlan)
    putStr     "How many reps? \t"
    reps <- Reps . read <$> getLine
    let
        finalWeight =
            apreSessionFinalSet apreSessionPlan reps
    putStrLn $ "Set #2:  \t\t" ++ show finalWeight
    putStr     "How many reps? \t"
    reps2 <- Reps . read <$> getLine
    pure $ runPlan apreSessionPlan reps reps2

runPlan :: ApreSessionPlan -> Reps -> Reps -> ApreSession
runPlan apreSessionPlan reps1 reps2 =
    ApreSession
        { plan = apreSessionPlan
        , set1Reps =
            Set
                { setWeight =
                    set1 apreSessionPlan
                , setReps =
                    reps1
                }
        , set2Reps =
            Set
                { setWeight =
                    apreSessionFinalSet apreSessionPlan reps1
                , setReps =
                    reps2
                }
        }

mkSessionPlan
    :: Weight
    -> Int
    -> Weight
    -> ApreSessionPlan
mkSessionPlan incr whichRm prevRm =
    ApreSessionPlan
        { warmup1 =
            Set
                { setWeight =
                    roundTo 2.5 $ prevRm * 0.5
                , setReps =
                    Reps $ min (whichRm * 2) (max (whichRm + 4) (whichRm + 2))
                }
        , warmup2 =
            Set
                { setWeight =
                    roundTo 2.5 $ prevRm * 0.75
                , setReps =
                    Reps whichRm
                }
        , set1 =
            prevRm
        , apreWhichRm =
            whichRm
        , apreIncrement =
            incr
        }
