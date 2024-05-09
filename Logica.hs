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

solve :: [(Expressao, Bool)] -> [(Expressao, Bool)] -> Bool
solve ((Variavel x, v) : exprs) vars = solve exprs (vars ++ [(Variavel x, v)])
--
solve ((Implicacao expX expY, True) : exprs) vars = solve (exprs ++ [(expX, False)]) vars && solve (exprs ++ [(expY, True)]) vars
solve ((Implicacao expX expY, False) : exprs) vars = solve (exprs ++ [(expX, True), (expY, False)]) vars
--
solve ((ELogico expX expY, True) : exprs) vars = solve (exprs ++ [(expX, True), (expY, True)]) vars
solve ((ELogico expX expY, False) : exprs) vars = solve (exprs ++ [(expX, False)]) vars && solve (exprs ++ [(expY, False)]) vars
--
solve ((OuLogico expX expY, True) : exprs) vars = solve (exprs ++ [(expX, True)]) vars && solve (exprs ++ [(expY, True)]) vars
solve ((OuLogico expX expY, False) : exprs) vars = solve (exprs ++ [(expX, False), (expY, False)]) vars
--
solve ((Negacao expX, v) : exprs) vars = solve (exprs ++ [(expX, not v)]) vars
--
solve [] vars = hasContradiction $ sortVars vars

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