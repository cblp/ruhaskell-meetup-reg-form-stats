{-# LANGUAGE NamedFieldPuns #-}

import Control.Arrow      ( (>>>) )
import Control.Exception  ( Exception, throw )
import Data.List.Split    ( splitOn, wordsBy )
import Data.Monoid        ( (<>) )
import Data.Set           as Set
import Data.Typeable      ( Typeable )

data Answer = Answer { name :: String, email :: String }

data Stats = Stats { namesAndEmailsAreUnique :: Bool }
    deriving Show

type Header = [String]

readAnswer :: Int -> String -> Answer
readAnswer lineNo tsvLine =
    case splitOn "\t" tsvLine of
        _ : name : email : _ ->
            Answer {name, email}
        badFields ->
            error $ "cannot read line " <> show badFields <> " at " <> show lineNo

readHeader :: String -> Header
readHeader = splitOn "\t"

readAnswers :: String -> [Answer]
readAnswers tsvContent =
    let headerLine : tsvLines = wordsBy (`elem` "\r\n") tsvContent
    in  assertEqual expectedHeader (readHeader headerLine)
            [readAnswer i line | (i, line) <- zip [1..] tsvLines]
  where
    expectedHeader =
        [ "Отметка времени"
        , "Имя и фамилия"
        , "E-mail"
        , "Ваш уровень знания Haskell"
        , "Чего вы ожидаете от митапа?"
        , "Компания"
        , "Должность"
        , "Используете ли Haskell или другие функциональные языки в работе?"
        , "Останетесь ли на неформальные посиделки?"
        , "Где следует проводить посиделки?"
        ]

assertEqual :: (Eq a, Show a, Typeable a) => a -> a -> b -> b
assertEqual expected got x =
    if expected == got
        then x
        else throw ExpectationFailed {expected, got}

stats :: [Answer] -> Stats
stats answers =
    let namesAndEmailsAreUnique =
            areUnique [(name, email) | Answer{name, email} <- answers]
    in  Stats { namesAndEmailsAreUnique }

main :: IO ()
main = interact $ readAnswers >>> stats >>> showLn

showLn :: Show a => a -> String
showLn = show >>> pure >>> unlines

areUnique :: Ord a => [a] -> Bool
areUnique = areUnique' Set.empty
  where
    areUnique' _      []      = True
    areUnique' known  (x:xs)  =
        x `Set.notMember` known && areUnique' (Set.insert x known) xs

data ExpectationFailed a = ExpectationFailed {expected :: a, got :: a}
    deriving Show
instance (Show a, Typeable a) => Exception (ExpectationFailed a)
