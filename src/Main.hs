{-# LANGUAGE NamedFieldPuns, TupleSections #-}

import            Control.Arrow         ( (>>>) )
import            Control.Monad         ( (>=>) )
import            Control.Monad.Writer  ( Writer, tell )
import            Control.Exception     ( Exception, throw )
import            Data.List             ( intercalate )
import            Data.List.Split       ( splitOn, wordsBy )
import            Data.Map              ( Map )
import qualified  Data.Map              as Map
import            Data.Maybe            ( catMaybes, mapMaybe )
import            Data.Monoid           ( (<>) )
import            Data.Set              ( Set )
import qualified  Data.Set              as Set
import            Data.Typeable         ( Typeable )
import            Text.Nicify           ( nicify )

data Answer = Answer  { name :: String
                      , email :: String
                      , haskellLevel :: Maybe HaskellLevel
                      , expectations :: Set Expectation
                      }

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

data HaskellLevel = Curious | Learning | Professional | Expert
    deriving (Eq, Ord, Show)

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

data Expectation  = Contacting
                  | Food
                  | GetHired
                  | GetKnowledge
                  | HaveFun
                  | ShareKnowledge
    deriving (Eq, Ord, Show)

data Problem = BadFields [String] | BadExpectation String
instance Show Problem where
    show (BadFields fields) = "BadFields " <> show fields
    show (BadExpectation expec) = "BadExpectation \"" <> expec <> "\""

readExpectations :: String -> Writer [Problem] (Set Expectation)
readExpectations "" = pure Set.empty
readExpectations s
    | s `elem`
        [ ")"
        , "быстро работало"
        , "все видео на youtube уже просмотрены"
        , "Да!"
        , "Да так"
        , "Давно слышал о haskell"
        , "занимаясь хаскеллем"
        , "зачем."
        , "и было надёжным"
        , "как"
        , "которые пишутся на Haskell"
        , "но более или менее продвинулся только после прохождения недавнего курса на stepic.org   Сейчас не очень понятно \"что делать дальше\""
        , "Не знаю"
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
        , "чтобы когда-нибудь смочь зарабатывать деньги"
        ]
      = pure Set.empty
    | s `elem`
        [ "болтовня"
        , " Возможность познакомиться заманчива"
        , "встретить интересных людей"
        , "и себя показать"
        , "кого-нибудь"
        , "Людей посмотреть"
        , "наверняка узнаю много нового для себя!"
        , "Найти единомышленников ))"
        , "Общение"
        , "Поговорить с интересными людьми."
        , "познакомиться"
        , "Познакомиться"
        , "Познакомиться с людьми"
        , "Познакомиться с реальными Haskell-разработчиками."
        , "Познакомиться с сообществом Хаскел"
        , "Познакомиться с хаскелистами"
        , "Пообщаться"
        , "пообщаться"
        , "Пообщаться с \"носителями языка\" Haskell"
        , "Пообщаться с профессиональными хаскелистами"
        , "Развиртуализироваться"
        , "Увидеть живых программистов на Haskell"
        , "узнать состояние дел в ру сообществе"
        ]
      = pure $ Set.singleton Contacting
    | s ==
        "печеньки"
      = pure $ Set.singleton Food
    | s `elem`
        [ "больше узнать о практическом опыте его применения"
        , "в очередной раз послушать про монады"
        , "Вдохновиться чем-нибудь"
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
        , "Послушать Зефирова"
        , "Послушать интересные доклады."
        , "послушать интересные штуки."
        , "послушать про использование haskell для решения всякого рода интересных задач"
        , "Посмотреть где в проде используется данный язык"
        , "Практических примеров использования Haskell"
        , "просветлиться"
        , "Расширить кругозор"
        , "решить проблемы" -- FIXME?
        , "узнать"
        , "узнать.."
        , "Узнать больше про Haskell"
        , "узнать новости с передовой"
        , "Узнать о системах"
        , "Узнать про FFI"
        , "Узнать что-нибудь"
        , "Узнать что-нибудь новое из области функционального программирования"
        , "Узнать что-то новое"
        , "Узнать что-то новое для себя"
        , "услышать познавательных докладов"
        , "Хочу узнать больше о Haskell"
        , "Хочу узнать больше о функциональном программировании"
        , "что вообще происходит в сообществе"
        , "что сейчас впринципе происходит в мире так"
        ]
      = pure $ Set.singleton GetKnowledge
    | s == "найти работу."
      = pure $ Set.singleton GetHired
    | s `elem`
        [ "Лулзов"
        , "Ощутить особую атмосферу хаскелистов"
        , "получить мощный заряд fun'а! :-D"
        , "флейм"
        ]
      = pure $ Set.singleton HaveFun
    | s `elem`
        [ "поделиться открытиями"
        , "Рассказать про создание хранилища для баз данных"
        ]
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
        haskellLevelDist = distributionExclusive $ mapMaybe haskellLevel answers
        expectationDist = distributionInclusive $ map expectations answers
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
