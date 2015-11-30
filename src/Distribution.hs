{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Distribution where

import            Data.List   ( intercalate )
import            Data.Map    ( Map )
import qualified  Data.Map    as Map
import            Data.Monoid ( (<>) )
import            Data.Set    ( Set )
import qualified  Data.Set    as Set

data Distribution a = Distribution { total :: Int, parts :: Map a Integer }

instance Show a => Show (Distribution a) where
    show Distribution{total, parts} = mconcat
        [ "["
        , intercalate ", "
              [ show k <> " = " <> show p <> "%"
              | (k, v) <- Map.toList parts
              , let p = if total == 0 then 0 else v * 100 // total
              ]
        , "]"
        ]
      where
        x // y = round (fromIntegral x / fromIntegral y :: Double) :: Integer
        infixl 7 //

distributionExclusive :: Ord a => [a] -> Distribution a
distributionExclusive xs = Distribution
    { total = length xs
    , parts = Map.fromListWith (+) $ fmap (, 1) xs
    }

distributionInclusive :: Ord a => [Set a] -> Distribution a
distributionInclusive votes = Distribution
    { total = length votes
    , parts = Map.fromListWith (+) $ concatMap (fmap (, 1) . Set.toList) votes
    }
