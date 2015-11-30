import Stats  ( stats )
import Voting ( readAnswers )

import Control.Arrow  ( (>>>) )
import Text.Nicify    ( nicify )

main :: IO ()
main = interact $ readAnswers >>> fmap stats >>> showLn >>> nicify

showLn :: Show a => a -> String
showLn = show >>> pure >>> unlines
