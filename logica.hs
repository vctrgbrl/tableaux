import Data.List

data Expressao = Variavel String | Implicacao Expressao Expressao | ELogico Expressao Expressao | OuLogico Expressao Expressao | Negacao Expressao

-- (p∨(q∧r))→((p∨q)∧(p∨r))
-- [(Implicacao (OuLogico (Variavel "p") (ELogico (Variavel "q") (Variavel "r"))) ( ELogico (OuLogico (Variavel "p" "q")) (OuLogico (Variavel "p" "r")) ), False)]
argument :: [(Expressao, Bool)]
-- argument = [(Variavel "a", True), (Implicacao (Variavel "a") (Variavel "b"), True), (Variavel "b", False)]
-- argument = [(Variavel "a", True), (Implicacao (Variavel "a") (Variavel "b"), True), (Variavel "b", False)]

argument = [(Implicacao (OuLogico (Variavel "p") (ELogico (Variavel "q") (Variavel "r"))) (ELogico (OuLogico (Variavel "p") (Variavel "q")) (OuLogico (Variavel "p") (Variavel "r"))), False)]

-- argument = [(Implicacao (Variavel "b") (ELogico (Variavel "a") (OuLogico (Variavel "b") (Variavel "a"))), False)]

evaluate :: [(Expressao, Bool)] -> [(Expressao, Bool)] -> Bool
evaluate ((Variavel x, v) : exprs) vars = evaluate exprs (vars ++ [(Variavel x, v)])
--
evaluate ((Implicacao expX expY, True) : exprs) vars = evaluate (exprs ++ [(expX, False)]) vars && evaluate (exprs ++ [(expY, True)]) vars
evaluate ((Implicacao expX expY, False) : exprs) vars = evaluate (exprs ++ [(expX, True), (expY, False)]) vars
--
evaluate ((ELogico expX expY, True) : exprs) vars = evaluate (exprs ++ [(expX, True), (expY, True)]) vars
evaluate ((ELogico expX expY, False) : exprs) vars = evaluate (exprs ++ [(expX, False)]) vars && evaluate (exprs ++ [(expY, False)]) vars
--
evaluate ((OuLogico expX expY, True) : exprs) vars = evaluate (exprs ++ [(expX, True)]) vars && evaluate (exprs ++ [(expY, True)]) vars
evaluate ((OuLogico expX expY, False) : exprs) vars = evaluate (exprs ++ [(expX, False), (expY, False)]) vars
--
evaluate ((Negacao expX, v) : exprs) vars = evaluate (exprs ++ [(expX, not v)]) vars
--
evaluate [] vars = hasContradiction $ sortVars vars

sortVars = sortBy (\(Variavel a, v) (Variavel b, g) -> compare a b)

contradiction :: (Expressao, Bool) -> (Expressao, Bool) -> Bool
contradiction (Variavel x, v) (Variavel y, w) = x == y && v == not w

hasContradiction :: [(Expressao, Bool)] -> Bool
hasContradiction [(Variavel x, value)] = False
hasContradiction ((Variavel x, value) : vars) = contradiction (Variavel x, value) (head vars) || hasContradiction vars