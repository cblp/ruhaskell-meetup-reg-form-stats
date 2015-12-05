import Stats  ( stats )
import Voting ( readAnswers )

import Data.Char      ( chr, isDigit )
import Text.Nicify    ( nicify )

main :: IO ()
main = interact $ nicify . fixShowLetters . showLn . fmap stats . readAnswers

showLn :: Show a => a -> String
showLn = unlines . pure . show

fixShowLetters :: String -> String
fixShowLetters s =
    let (str, buf) = foldr f ("", "") s
    in  buf ++ str
  where
    f '\\'  (str, ""  )             = ('\\':str,              "")
    f '\\'  (str, buf )             = (chr (read buf) : str,  "")
    f c     (str, buf ) | isDigit c = (str,                   c:buf)
                        | otherwise = (c : buf ++ str,        "")
