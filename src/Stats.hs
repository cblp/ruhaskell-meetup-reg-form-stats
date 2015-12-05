{-# LANGUAGE NamedFieldPuns #-}

module Stats where

import Distribution ( Distribution
                    , distributionExclusive
                    , distributionInclusive
                    )
import Voting       ( Answer(..), Expectation, HaskellLevel )

import Data.Maybe ( mapMaybe )

data Stats = Stats  { count :: Int
                    , haskellLevelDist :: Distribution HaskellLevel
                    , expectationDist :: Distribution Expectation
                    }
    deriving Show

stats :: [Answer] -> Stats
stats answers =
    let count = length answers
        haskellLevelDist = distributionExclusive $ mapMaybe haskellLevel answers
        expectationDist = distributionInclusive $ mapMaybe expectations answers
    in  Stats { count
              , haskellLevelDist
              , expectationDist
              }
