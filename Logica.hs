module Logica
  ( solve,
    Expressao (Variavel, Implicacao, ELogico, OuLogico, Negacao),
  )
where

import Data.List

data Expressao = Variavel String | Implicacao Expressao Expressao | ELogico Expressao Expressao | OuLogico Expressao Expressao | Negacao Expressao

-- (p∨(q∧r))→((p∨q)∧(p∨r))
-- → ∨ p ∧ q r ∧ ∨ p q ∨ p r
-- (a→b)
-- [(Implicacao (OuLogico (Variavel "p") (ELogico (Variavel "q") (Variavel "r"))) ( ELogico (OuLogico (Variavel "p" "q")) (OuLogico (Variavel "p" "r")) ), False)]
argument :: [(Expressao, Bool)]
-- argument = [(Variavel "a", True), (Implicacao (Variavel "a") (Variavel "b"), True), (Variavel "b", False)]
-- argument = [(Variavel "a", True), (Implicacao (Variavel "a") (Variavel "b"), True), (Variavel "b", False)]

argument = [(Implicacao (OuLogico (Variavel "p") (ELogico (Variavel "q") (Variavel "r"))) (ELogico (OuLogico (Variavel "p") (Variavel "q")) (OuLogico (Variavel "p") (Variavel "r"))), False)]

-- argument = [(Implicacao (Variavel "b") (ELogico (Variavel "a") (OuLogico (Variavel "b") (Variavel "a"))), False)]
solve :: [(Expressao, Bool)] -> (Bool, [Char])
solve exprs = _solve exprs [] ""

_solve :: [(Expressao, Bool)] -> [(Expressao, Bool)] -> [Char] -> (Bool, [Char])
_solve ((Variavel x, v) : exprs) vars num = do
  let res = num ++ "" ++ show (Variavel x, v)
  let (a, b) = _solve exprs (vars ++ [(Variavel x, v)]) num
  (a, res ++ "\n" ++ b)
--
_solve ((Implicacao expX expY, True) : exprs) vars num = do
  let res = num ++ "" ++ show (Implicacao expX expY, True)
  let (a, b) = _solve (exprs ++ [(expX, False)]) vars (num ++ "\t")
  let (c, d) = _solve (exprs ++ [(expY, True)]) vars (num ++ "\t")
  (a && c, res ++ "\n" ++ b ++ "\n" ++ d)
_solve ((Implicacao expX expY, False) : exprs) vars num = do
  let res = num ++ "" ++ show (Implicacao expX expY, False)
  let (a, b) = _solve (exprs ++ [(expX, True), (expY, False)]) vars num
  (a, res ++ "\n" ++ b)
--
_solve ((ELogico expX expY, True) : exprs) vars num = do
  let res = num ++ "" ++ show (ELogico expX expY, True)
  let (a, b) = _solve (exprs ++ [(expX, True), (expY, True)]) vars num
  (a, res ++ "\n" ++ b)
_solve ((ELogico expX expY, False) : exprs) vars num = do
  let res = num ++ "" ++ show (ELogico expX expY, False)
  let (a, b) = _solve (exprs ++ [(expX, False)]) vars (num ++ "\t")
  let (c, d) = _solve (exprs ++ [(expY, False)]) vars (num ++ "\t")
  (a && c, res ++ "\n" ++ b ++ "\n" ++ d)
--
_solve ((OuLogico expX expY, True) : exprs) vars num = do
  let res = num ++ "" ++ show (OuLogico expX expY, True)
  let (a, b) = _solve (exprs ++ [(expX, True)]) vars (num ++ "\t")
  let (c, d) = _solve (exprs ++ [(expY, True)]) vars (num ++ "\t")
  (a && c, res ++ "\n" ++ b ++ "\n" ++ d)
_solve ((OuLogico expX expY, False) : exprs) vars num = do
  let res = num ++ "" ++ show (OuLogico expX expY, False)
  let (a, b) = _solve (exprs ++ [(expX, False), (expY, False)]) vars num
  (a, res ++ "\n" ++ b)
--
_solve ((Negacao expX, v) : exprs) vars num = do
  let res = num ++ "" ++ show (Negacao expX, v)
  let (a, b) = _solve (exprs ++ [(expX, not v)]) vars num
  (a, res ++ "\n" ++ b)
--
_solve [] vars num = (hasContradiction $ sortVars vars, "")

sortVars = sortBy (\(Variavel a, v) (Variavel b, g) -> compare a b)

contradiction :: (Expressao, Bool) -> (Expressao, Bool) -> Bool
contradiction (Variavel x, v) (Variavel y, w) = x == y && v == not w

hasContradiction :: [(Expressao, Bool)] -> Bool
hasContradiction [(Variavel x, value)] = False
hasContradiction ((Variavel x, value) : vars) = contradiction (Variavel x, value) (head vars) || hasContradiction vars

instance Show Expressao where
  show (Variavel x) = "(Variável " ++ show x ++ ")"
  show (Implicacao x y) = "(Implicação " ++ show x ++ " " ++ show y ++ ")"
  show (ELogico x y) = "(ELogico " ++ show x ++ " " ++ show y ++ ")"
  show (OuLogico x y) = "(OuLogico " ++ show x ++ " " ++ show y ++ ")"
  show (Negacao x) = "(Negacao " ++ show x ++ " " ++ ")"