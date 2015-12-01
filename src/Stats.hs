{-# LANGUAGE NamedFieldPuns #-}

module Stats where

import Distribution ( Distribution
                    , distributionExclusive
                    , distributionInclusive
                    )
import Voting       ( Answer(..), Expectation, HaskellLevel )

import            Data.Maybe ( mapMaybe )
import qualified  Data.Set   as Set

data Stats = Stats  { count :: Int
                    , namesAndEmailsAreUnique :: Bool
                    , haskellLevelDist :: Distribution HaskellLevel
                    , expectationDist :: Distribution Expectation
                    }
    deriving Show

stats :: [Answer] -> Stats
stats answers =
    let count = length answers
        namesAndEmailsAreUnique =
            areUnique [(name, email) | Answer{name, email} <- answers]
        haskellLevelDist = distributionExclusive $ mapMaybe haskellLevel answers
        expectationDist = distributionInclusive $ mapMaybe expectations answers
    in  Stats { count
              , namesAndEmailsAreUnique
              , haskellLevelDist
              , expectationDist
              }

areUnique :: Ord a => [a] -> Bool
areUnique = areUnique' Set.empty
  where
    areUnique' _      []      = True
    areUnique' known  (x:xs)  =
        x `Set.notMember` known && areUnique' (Set.insert x known) xs
