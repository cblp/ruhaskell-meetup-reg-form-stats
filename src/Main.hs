{-# LANGUAGE NamedFieldPuns #-}

import Control.Arrow      ( (>>>) )
import Control.Exception  ( Exception, throw )
import Data.List          ( intercalate )
import Data.List.Split    ( splitOn, wordsBy )
import Data.Map           as Map
import Data.Monoid        ( (<>) )
import Data.Set           as Set
import Data.Typeable      ( Typeable )
import Text.Nicify        ( nicify )

data Answer = Answer  { name :: String
                      , email :: String
                      , haskellLevel :: Maybe HaskellLevel
                      }

newtype Distribution a = Distribution (Map a Integer)

instance Show a => Show (Distribution a) where
    show (Distribution m) =
        let total = sum $ Map.elems m
        in  concat
                [ "["
                , intercalate ", "
                      [ show k <> " = " <> show p <> "%"
                      | (k, v) <- Map.toList m
                      , let p = if total == 0 then 0 else v * 100 // total
                      ]
                , "]"
                ]
      where
        (//) :: Integer -> Integer -> Integer
        x // y = round (fromIntegral x / fromIntegral y :: Double)
        infixl 7 //

data HaskellLevel = Curious | Learning | Professional | Expert
    deriving (Eq, Ord, Show)

readHaskellLevel :: String -> String -> Maybe HaskellLevel
readHaskellLevel _ ""                     = Nothing
readHaskellLevel _ "интересующийся"       = Just Curious
readHaskellLevel _ "изучающий"            = Just Learning
readHaskellLevel _ "начинающий"           = Just Learning
readHaskellLevel _ "профессионал (знаю достаточно для практического применения)"
                                          = Just Professional
readHaskellLevel _ "эксперт (знаю много)" = Just Expert
readHaskellLevel email s =
    error $ concat  [ "readHaskellLevel: can't understand level \""
                    , s
                    , "\" for user "
                    , email
                    ]

data Stats = Stats  { count :: Int
                    , namesAndEmailsAreUnique :: Bool
                    , haskellLevelDistribition :: Distribution HaskellLevel
                    }
    deriving Show

type Header = [String]

readAnswer :: Int -> String -> Answer
readAnswer lineNo tsvLine =
    case splitOn "\t" tsvLine of
        _ : name : email : haskellLevelStr : _ ->
            let haskellLevel = readHaskellLevel email haskellLevelStr
            in  Answer {name, email, haskellLevel}
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
    let count = length answers
        namesAndEmailsAreUnique =
            areUnique [(name, email) | Answer{name, email} <- answers]
        haskellLevelDistribition = Distribution $
            Map.fromListWith (+)  [ (hl, 1)
                                  | Answer{haskellLevel} <- answers
                                  , Just hl <- pure haskellLevel
                                  ]
    in  Stats { count, namesAndEmailsAreUnique, haskellLevelDistribition }

main :: IO ()
main = interact $ readAnswers >>> stats >>> showLn >>> nicify

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
