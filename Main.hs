import System.Environment
import System.IO
import Util (split)

import Parser (tokenize, infixToPrefix, parse)
import Logica (solve)

addBool [c] = [(c, False)]
addBool (c:xs) = (c, True):addBool xs

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  let a = solve $ addBool $ map (fst . parse . infixToPrefix . tokenize) (split contents '\n')
  print (fst a)
  writeFile "output.txt" (snd a)
  hClose handle