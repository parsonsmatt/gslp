{-# LANGUAGE TypeApplications, GADTs, FlexibleInstances, OverloadedLists #-}

module History where

import Lib
import Control.Monad (join)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (Day, fromGregorian)
import qualified Data.List as List

history :: NonEmpty Session
history =
    [ 2019 05 07 #:
        [ bench press %:
            155 x 5 x 5
        , deadlift %:
            [ 225 x 5
            , 275 x 3
            ] <>
            315 x 1 x 3
        , db rows %:
            45 x 12 x 6
        ]
    , 2019 04 15 #:
        [ press %:
            100 x 5 x 5
        , deadlift %:
            225 x 5 x 3
        , bb curl %:
            45 x 20 x 4
        ]

    , 2019 04 11 #:
        [ bench press %:
            [ 155 x 5
            , 165 x 3
            , 175 x 2
            , 185 x 1
            , 195 x 1
            , 205 x 1
            , 185 x 3
            , 155 x 5
            , 155 x 5
            , 155 x 5
            , 135 x 10
            ]
        , squat %:
            [ 155 x 5
            , 185 x 3
            , 195 x 1
            , 205 x 1
            , 215 x 1
            , 225 x 1
            , 235 x 1
            ]
        , bb curls %:
            45 x 15 x 3
        ]

    , 2019 04 08 #:
        [ press %:
            100 x 5 x 3
            <>
            [ 115 x 3
            , 125 x 3
            , 135 x 2
            , 115 x 3
            , 95 x 5
            ]
        , deadlift %:
            [ 135 x 5
            , 185 x 5
            , 225 x 5
            , 245 x 2
            , 265 x 2
            , 275 x 1
            , 295 x 1
            , 315 x 1
            ]
        , bb curls %:
            45 x 20 x 3

    , 2019 04 01 #:
        [ bench press %:
            [ 135 x 5
            , 145 x 5
            , 155 x 5
            ] <> 165 x 5 x 3 <>
            [ 155 x 5
            , 135 x 5
            ]
        , squat %:
            185 x 5 x 3
        ]

    , 2019 03 30 #:
        [ press %:
            95 x 5 x 2
            <>
            [ 95 x 8
            , 105 x 3
            , 115 x 3
            , 125 x 1
            , 135 x 1
            ]
        , squat %:
            [ 150 x 5
            , 165 x 5
            , 185 x 5
            , 205 x 5
            , 225 x 5
            ]
        , db curl %:
            20 x 15 x 4
        ]

    , 2019 03 27 #:
        [ bench press %:
            125 x 5 x 2
            <>
            125 x 8 x 1
        , deadlift %:
            [ 145 x 5
            , 155 x 5
            , 165 x 5
            , 175 x 5
            ]
        , db press %:
            30 x 12 x 4
        , db row %:
            50 x 12 x 4
        , latPull %:
            [ 60 x 10
            , 70 x 10
            , 80 x 10
            , 90 x 10
            , 100 x 10
            ]
        ]

    , 2019 03 25 #:
        [ squat %:
            145 x 5 x 2
            <>
            145 x 8 x 1
        , press %:
            90 x 5 x 2
            <>
            90 x 9 x 1
        , curl %:
            55 x 12 x 4
        , press %:
            55 x 12 x 4
        ]

    , 2019 03 23 #:
        [ squat %:
            140 x 5 x 2
            <>
            140 x 8 x 1
        , bench press %:
            120 x 5 x 2
            <>
            120 x 8 x 1
        , db row %:
            50 x 12 x 4
        , db press %:
            25 x 12 x 4
        ]

    , 2019 03 19 #:
        [ press %:
            85 x 5 x 2
            <>
            85 x 12 x 1
        , deadlift %:
            135 x 5 x 1
        , reverseHyper %:
            20 x 12 x 3
        ]

    , 2019 03 17 #:
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
    ]
