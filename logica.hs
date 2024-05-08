-- {(a), (a, implica, b), (b)}
-- {(a,true), (a, implica, b), (b, false)}
-- {(a, true), (a, implica, b), (b, false), (a, false)} -> x
-- {(a, true), (a, implica, b), (b,false), (b, true)} -> x

import Data.List

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact (x - 1)

data Expressao = Variavel String | Implicacao String String

argument :: [(Expressao, Bool)]
-- argument = [(Variavel "a", True), (Implicacao (Variavel "a") (Variavel "b"), True), (Variavel "b", False)]
argument = [(Variavel "a", True), (Implicacao "a" "b", True), (Variavel "b", False)]

evaluate :: [(Expressao, Bool)] -> [(Expressao, Bool)] -> Bool
evaluate ((Variavel x, v) : xs) vars = evaluate xs (vars ++ [(Variavel x, v)])
evaluate ((Implicacao x y, True) : xs) vars = evaluate xs (vars ++ [(Variavel x, False)]) && evaluate xs (vars ++ [(Variavel y, True)])
evaluate ((Implicacao x y, False) : xs) vars = evaluate xs (vars ++ [(Variavel x, True), (Variavel y, False)])
evaluate [] vars = hasContradiction $ sortVars vars

sortVars = sortBy (\(Variavel a, v) (Variavel b, g) -> compare a b)

contradiction :: (Expressao, Bool) -> (Expressao, Bool) -> Bool
contradiction (Variavel x, v) (Variavel y, w) = x == y && v == not w

hasContradiction :: [(Expressao, Bool)] -> Bool
hasContradiction [(Variavel x, value)] = False
hasContradiction ((Variavel x, value) : vars) = contradiction (Variavel x, value) (head vars) || hasContradiction vars