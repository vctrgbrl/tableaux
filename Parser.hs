module Parser
  ( tokenize,
    infixToPrefix,
    parse,
  )
where

import Data.List (sortBy)
import Logica (Expressao (ELogico, Implicacao, Negacao, OuLogico, Variavel), solve)

data Token = Var String | Imp | ELog | OuLog | Neg | InitExp | EndExp

parse :: [Token] -> (Expressao, [Token])
parse (Imp : xs) = do
  let (expA, restA) = parse xs
  let (expB, restB) = parse restA
  (Implicacao expA expB, restB)
parse (OuLog : xs) = do
  let (expA, restA) = parse xs
  let (expB, restB) = parse restA
  (OuLogico expA expB, restB)
parse (ELog : xs) = do
  let (expA, restA) = parse xs
  let (expB, restB) = parse restA
  (ELogico expA expB, restB)
parse (Neg : xs) = do
  let (expA, restA) = parse xs
  (Negacao expA, restA)
parse ((Var x) : xs) = (Variavel x, xs)

--
-- parse (Variavel : xs) parsingVar =
--   if parsingVar == ""
--     then parse xs ""
--     else (Variavel parsingVar, xs)
-- parse (varChar : xs) parsingVar = parse xs (parsingVar ++ [varChar])

infixToPrefix :: [Token] -> [Token]
infixToPrefix tokens = map fst (sortToken $ evaluateToken tokens 1 "")

evaluateToken :: [Token] -> Int -> String -> [(Token, String)]
evaluateToken (InitExp : xs) c s = evaluateToken xs 1 (s ++ show c)
evaluateToken [EndExp] c s = []
evaluateToken (EndExp : xs) c s = evaluateToken xs c (init s)
evaluateToken (exp : xs) 1 s = (exp, s ++ "1") : evaluateToken xs 0 s
evaluateToken (exp : xs) 0 s = (exp, s ++ "0") : evaluateToken xs 2 s
evaluateToken (exp : xs) 2 s = (exp, s ++ "2") : evaluateToken xs 0 s

sortToken :: [(Token, String)] -> [(Token, String)]
sortToken = sortBy (\(token, strA) (tokenB, strB) -> compare strA strB)

tokenize :: String -> [Token]
tokenize str = _tokenize (filter (/= ' ') str) ""

_tokenize :: String -> String -> [Token]
-- tokenize t = tokenize $ filter (== ' ') t
_tokenize [] "" = []
_tokenize (c : xs) ""
  | c == '(' = InitExp : _tokenize xs ""
  | c == ')' = EndExp : _tokenize xs ""
  | c == '→' = Imp : _tokenize xs ""
  | c == '∧' = ELog : _tokenize xs ""
  | c == '∨' = OuLog : _tokenize xs ""
  | c == '¬' = Neg : _tokenize xs ""
_tokenize (c : xs) parsingVar
  | c == '(' = [Var parsingVar, InitExp] ++ _tokenize xs ""
  | c == ')' = [Var parsingVar, EndExp] ++ _tokenize xs ""
  | c == '→' = [Var parsingVar, Imp] ++ _tokenize xs ""
  | c == '∧' = [Var parsingVar, ELog] ++ _tokenize xs ""
  | c == '∨' = [Var parsingVar, OuLog] ++ _tokenize xs ""
  | c == '¬' = [Var parsingVar, Neg] ++ _tokenize xs ""
_tokenize (varChar : xs) parsingVar = _tokenize xs (parsingVar ++ [varChar])

-- nomeArquivo:String -> readFile:String -> breakLines each [String]  -> tokenize:[Token] -> infixToPrefix: [Token] -> parse: Expression
instance Show Token where
  show (Var x) = show x
  show Imp = "→"
  show ELog = " ∧ "
  show OuLog = " ∨ "
  show Neg = " ¬ "
  show InitExp = "("
  show EndExp = ")"