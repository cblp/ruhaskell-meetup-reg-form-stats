{-# LANGUAGE NamedFieldPuns, TupleSections #-}

import            Control.Arrow     ( (>>>) )
import            Control.Monad     ( (>=>) )
import            Control.Exception ( Exception, throw )
import            Data.List         ( intercalate )
import            Data.List.Split   ( splitOn, wordsBy )
import            Data.Map          ( Map )
import qualified  Data.Map          as Map
import            Data.Maybe        ( mapMaybe )
import            Data.Monoid       ( (<>) )
import            Data.Set          ( Set )
import qualified  Data.Set          as Set
import            Data.Typeable     ( Typeable )
import            Text.Nicify       ( nicify )

data Answer = Answer  { name :: String
                      , email :: String
                      , haskellLevel :: Maybe HaskellLevel
                      , expectations :: Set Expectation
                      }

newtype Distribution a = Distribution (Map a Integer)

instance Show a => Show (Distribution a) where
    show (Distribution m) =
        let total = sum $ Map.elems m
        in  mconcat
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

distribution :: Ord a => [a] -> Distribution a
distribution = Distribution . Map.fromListWith (+) . fmap (, 1)

readHaskellLevel :: String -> Maybe HaskellLevel
readHaskellLevel ""                     = Nothing
readHaskellLevel "интересующийся"       = Just Curious
readHaskellLevel "изучающий"            = Just Learning
readHaskellLevel "начинающий"           = Just Learning
readHaskellLevel "профессионал (знаю достаточно для практического применения)"
                                        = Just Professional
readHaskellLevel "эксперт (знаю много)" = Just Expert
readHaskellLevel s =
    error $ mconcat [ "readHaskellLevel: can't understand level \""
                    , s
                    , "\""
                    ]

data Expectation = GetKnowledge | MeetHaskellists | ShareKnowledge
    deriving (Eq, Ord, Show)

readExpectations :: String -> Set Expectation
readExpectations "" = Set.empty
readExpectations s
    | s `elem`
        ["Да так, чтобы", "быстро работало,", "было надёжным", "с ума не сойти, реализуя."]
      = Set.empty
    | s `elem`
        [ "Хочу узнать больше о Haskell"
        , "услышать познавательных докладов"
        ]
      = Set.singleton GetKnowledge
    | s `elem`
        [ "встретить интересных людей"
        , "Познакомиться с хаскелистами, наверняка узнаю много нового для себя!"
        ]
      = Set.singleton MeetHaskellists
    | s `elem`
        ["Рассказать про создание хранилища для баз данных"]
      = Set.singleton ShareKnowledge
    | otherwise
      = let ss = (splitOn " и " >=> splitOn ". ") s
            n = length ss
        in  case n of
                1 -> error $ mconcat
                    [ "readExpectations: can't understand expectations \""
                    , s
                    , "\""
                    ]
                _ -> Set.unions $ fmap readExpectations ss

data Stats = Stats  { count :: Int
                    , namesAndEmailsAreUnique :: Bool
                    , haskellLevelDist :: Distribution HaskellLevel
                    , expectationDist :: Distribution Expectation
                    }
    deriving Show

type Header = [String]

readAnswer :: Int -> String -> Answer
readAnswer lineNo tsvLine =
    case splitOn "\t" tsvLine of
        _ : name : email : haskellLevelStr : expectationsStr : _ ->
            let haskellLevel = readHaskellLevel haskellLevelStr
                expectations = readExpectations expectationsStr
            in  Answer {name, email, haskellLevel, expectations}
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
        haskellLevelDist = distribution $ mapMaybe haskellLevel answers
        expectationDist = distribution $
            concatMap (Set.toList . expectations) answers
    in  Stats { count
              , namesAndEmailsAreUnique
              , haskellLevelDist
              , expectationDist
              }

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
