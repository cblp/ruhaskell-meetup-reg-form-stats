{-# LANGUAGE NamedFieldPuns, TupleSections #-}

import            Control.Arrow     ( (>>>) )
import            Control.Monad     ( (>=>) )
import            Control.Monad.Writer ( Writer, tell )
import            Control.Exception ( Exception, throw )
import            Data.List         ( intercalate )
import            Data.List.Split   ( splitOn, wordsBy )
import            Data.Map          ( Map )
import qualified  Data.Map          as Map
import            Data.Maybe        ( catMaybes, mapMaybe )
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

data Expectation = Contacting | Food | GetKnowledge | HaveFun | ShareKnowledge
    deriving (Eq, Ord, Show)

data Problem = BadFields [String] | BadExpectation String
    deriving Show

readExpectations :: String -> Writer [Problem] (Set Expectation)
readExpectations "" = pure Set.empty
readExpectations s
    | s `elem`
        [ ")"
        , "быстро работало"
        , "Да!"
        , "Да так"
        , "Давно слышал о haskell"
        , "зачем."
        , "и было надёжным"
        , "как"
        , "которые пишутся на Haskell"
        , "но более или менее продвинулся только после прохождения недавнего курса на stepic.org   Сейчас не очень понятно \"что делать дальше\""
        , "Немного программировал на scala"
        , "но не кажется очень вероятной."
        , "реализуя."
        , "с которыми сталкиваются промышленные разработчики на полностью функциональных языках программирования."
        , "получить представление о том"
        , "предпринимал несколько попыток изучить"
        , "с ума не сойти"
        , "Типа того"
        , "узнать чем живут"
        , "Хотелось бы узнать где"
        , "что используют"
        , "чтобы"
        ]
      = pure Set.empty
    | s `elem`
        [ "болтовня"
        , " Возможность познакомиться заманчива"
        , "встретить интересных людей"
        , "наверняка узнаю много нового для себя!"
        , "Людей посмотреть", "и себя показать"
        , "Общение"
        , "Поговорить с интересными людьми."
        , "познакомиться"
        , "Познакомиться"
        , "Познакомиться с реальными Haskell-разработчиками."
        , "Познакомиться с сообществом Хаскел"
        , "Познакомиться с хаскелистами"
        , "Пообщаться"
        , "Пообщаться с \"носителями языка\" Haskell"
        , "Пообщаться с профессиональными хаскелистами"
        , "Увидеть живых программистов на Haskell"
        , "узнать состояние дел в ру сообществе"
        ]
      = pure $ Set.singleton Contacting
    | s `elem`
        ["печеньки"]
      = pure $ Set.singleton Food
    | s `elem`
        [ "в очередной раз послушать про монады"
        , "где он используется"
        , "и надеюсь что доклады на митапе как дадут представление о том"
        , "как используют Haskell для реальных задач."
        , "какие проблемы с ним возникают"
        , "Побольше узнать о проблемах"
        , "подскажут чем интересным можно заняться для дальнейшего постижения языка"
        , "...подход к проруби...:*))"
        , "получить информацию о новых технологиях"
        , "пообщаться на технические темы"
        , "Послушать"
        , "Послушать интересные доклады."
        , "послушать интересные штуки."
        , "послушать про использование haskell для решения всякого рода интересных задач"
        , "просветлиться"
        , "Расширить кругозор"
        , "узнать"
        , "узнать новости с передовой"
        , "Узнать о системах"
        , "Узнать про FFI"
        , "Узнать что-нибудь новое из области функционального программирования"
        , "Узнать что-то новое для себя"
        , "услышать познавательных докладов"
        , "Хочу узнать больше о Haskell"
        , "Хочу узнать больше о функциональном программировании"
        , "что сейчас впринципе происходит в мире так"
        ]
      = pure $ Set.singleton GetKnowledge
    | s `elem`
        ["получить мощный заряд fun'а! :-D"]
      = pure $ Set.singleton HaveFun
    | s `elem`
        ["Рассказать про создание хранилища для баз данных"]
      = pure $ Set.singleton ShareKnowledge
    | otherwise
      = let ss = (splitOn ". " >=> splitOn ", " >=> splitOn " и ") s
            n = length ss
        in  case n of
                1 -> do
                    tell [BadExpectation s]
                    pure Set.empty
                _ -> Set.unions <$> mapM readExpectations ss

data Stats = Stats  { count :: Int
                    , namesAndEmailsAreUnique :: Bool
                    , haskellLevelDist :: Distribution HaskellLevel
                    , expectationDist :: Distribution Expectation
                    }
    deriving Show

type Header = [String]

readAnswer :: String -> Writer [Problem] (Maybe Answer)
readAnswer tsvLine =
    case splitOn "\t" tsvLine of
        _ : name : email : haskellLevelStr : expectationsStr : _ -> do
            let haskellLevel = readHaskellLevel haskellLevelStr
            expectations <- readExpectations expectationsStr
            pure $ Just Answer {name, email, haskellLevel, expectations}
        badFields -> do
            tell [BadFields badFields]
            pure Nothing

readHeader :: String -> Header
readHeader = splitOn "\t"

readAnswers :: String -> Writer [Problem] [Answer]
readAnswers tsvContent =
    let headerLine : tsvLines = wordsBy (`elem` "\r\n") tsvContent
    in  assertEqual expectedHeader (readHeader headerLine)
            (catMaybes <$> mapM readAnswer tsvLines)
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
main = interact $ readAnswers >>> fmap stats >>> showLn >>> nicify

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
