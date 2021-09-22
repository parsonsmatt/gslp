{-# language TypeApplications #-}
module Chart where

import qualified Data.Map as Map
import Data.Foldable
import Control.Monad
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time

import History
import Lib

-- liftProgress :: IO ()
-- liftProgress = toFile def "list-history.png" $ do
--     layout_title .= "Lift History"
--     layout_x_axis . laxis_title .= "Date"
--     layout_y_axis . laxis_title .= "Weight"
--     forM_ [squat, bench press, deadlift, press] $ \liftName ->
--         plot
--         . line liftName
--         . pure
--         . (fmap . fmap) (maximum . fmap e1rm . concatMap (toList . liftSets) . toList)
--         . Map.toList
--         . onlyLiftsOf liftName
--         . dateLifts
--         $ history
