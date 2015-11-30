{-# LANGUAGE NamedFieldPuns #-}

module Voting where

import Common ( Problem(..), assertEqual )

import            Control.Monad         ( (>=>) )
import            Control.Monad.Writer  ( Writer, tell )
import            Data.List.Split       ( splitOn, wordsBy )
import            Data.Maybe            ( catMaybes )
import            Data.Set              ( Set )
import qualified  Data.Set              as Set

data HaskellLevel = Curious | Learning | Professional | Expert
    deriving (Eq, Ord, Show)

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

type Header = [String]

readHeader :: String -> Header
readHeader = splitOn "\t"

data Answer = Answer  { name :: String
                      , email :: String
                      , haskellLevel :: Maybe HaskellLevel
                      , expectations :: Set Expectation
                      }

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
