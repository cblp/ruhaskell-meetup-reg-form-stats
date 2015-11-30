{-# LANGUAGE NamedFieldPuns #-}

module Common where

import Control.Exception  ( Exception, throw )
import Data.Monoid        ( (<>) )
import Data.Typeable      ( Typeable )

data Problem = BadFields [String] | BadExpectation String

instance Show Problem where
    show (BadFields fields) = "BadFields " <> show fields
    show (BadExpectation expec) = "BadExpectation \"" <> expec <> "\""

data ExpectationFailed a = ExpectationFailed {expected :: a, got :: a}
    deriving Show

instance (Show a, Typeable a) => Exception (ExpectationFailed a)

assertEqual :: (Eq a, Show a, Typeable a) => a -> a -> b -> b
assertEqual expected got x =
    if expected == got
        then x
        else throw ExpectationFailed {expected, got}
