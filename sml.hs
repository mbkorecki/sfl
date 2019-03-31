import Data.Char

type Name   = String
type Relop  = String

data Expr = Val Integer
          | Ident Name [Expr]
          | Conditional Expr Relop Expr Expr Expr
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :%: Expr


pars :: String -> String
pars s = "(" ++ s ++ ")"

helpShow :: [Expr] -> String
helpShow [] = []
helpShow (e:ex) | length ex == 0 = show e
                | otherwise      = show e  ++ " " ++ helpShow ex

instance Show Expr where
  show (Val n) = show n
  show (Ident id ex) = id ++ "{" ++ helpShow ex ++ "}"
  show (Conditional lhs op rhs e0 e1) =
    "[" ++ show lhs ++ op ++ show rhs ++ " ? " ++ show e0 ++ " : " ++ show e1 ++ "]"
  show (e0 :+: e1) = pars(show e0 ++ " + " ++ show e1)
  show (e0 :-: e1) = pars(show e0 ++ " - " ++ show e1)
  show (e0 :*: e1) = pars(show e0 ++ "*" ++ show e1)
  show (e0 :/: e1) = pars(show e0 ++ "/" ++ show e1)
  show (e0 :%: e1) = pars(show e0 ++ "%" ++ show e1)

  -- Expr  -> T E’
  -- E’    -> + T E’ | - T E’ | <empty string>
  -- T     -> F T’
  -- T’    -> * F T’ | / F T’ | % F T’ | <empty string>
  -- F     -> [ Expr Rel Expr ? Expr : Expr]
  -- | ( Expr )
  -- | <integer>
  -- | <identifier>
  -- Rel   -> < | <= | = | >= | > | <>

lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c " \t.\n"      = lexer cs   -- skip spaces and tabs
  | elem c "*/+-()%[],>" = [c]:(lexer cs)
  | elem c "<>=?:{}^"    = [c]:(lexer cs)
  | isAlpha c            = (c:takeWhile isAlpha cs): lexer(dropWhile isAlpha cs)
  | isDigit c            = (c:takeWhile isDigit cs): lexer(dropWhile isDigit cs)
  | otherwise            = [error "Syntax Error: invalid character in input"]

parseE :: Expr -> [String] -> (Expr,[String])
parseE accepted tokens = parseE' acc rest
  where (acc, rest) = parseT accepted tokens

parseE' :: Expr -> [String] -> (Expr,[String])
parseE' accepted ("+":tokens) =
  let (acc,rest) = parseT (accepted) tokens
      in parseE' (accepted :+: acc) rest
parseE' accepted ("-":tokens) =
  let (acc,rest) = parseT (accepted) tokens
      in parseE' (accepted :-: acc) rest
parseE' accepted (")":tokens) = (accepted, tokens)
parseE' accepted ("]":tokens) = (accepted, "]":tokens)
parseE' accepted tokens = (accepted, tokens)

parseT :: Expr -> [String] -> (Expr,[String])
parseT accepted tokens = parseT' acc rest
  where (acc, rest) = parseF accepted tokens

parseT' :: Expr -> [String] -> (Expr,[String])
parseT' accepted ("*":tokens) =
  let (acc,rest) = parseF (accepted) tokens
    in parseT' (accepted :*: acc) rest
parseT' accepted ("/":tokens) =
  let (acc,rest) = parseF (accepted) tokens
    in parseT' (accepted :/: acc) rest
parseT' accepted ("%":tokens) =
  let (acc,rest) = parseF (accepted) tokens
    in parseT' (accepted :%: acc) rest
parseT' accepted (")":tokens) = (accepted, tokens)
parseT' accepted ("]":tokens) = (accepted, "]":tokens)
parseT' accepted tokens = (accepted, tokens)

parseF :: Expr -> [String] -> (Expr, [String])
parseF accepted []      =  error "Parse error at end of input...abort"
parseF accepted (tok:tokens)
  | isAlpha (head tok) && (length tokens == 0 || ((head tokens) /= "{"))
                            = (Ident tok [], tokens)
  | isAlpha (head tok)      = (Ident tok a5, t5)
  | isDigit (head tok)      = (Val (read (tok)), tokens)
  | head tok == '['         = parseT condAcc condToks
  | head tok == ']'         = (accepted, tokens)
  | elem (head tok) "?:("   = parseE accepted tokens
  | otherwise               = (accepted, tok:tokens)
    where
      (condAcc, condToks) = (Conditional a4 a3 a2 a1 acc, toks)
      (acc, toks)  = parseE a1 t1
      (a1, t1)     = parseE a2 t2
      (a2, t2)     = parseE a4 t3
      (a3, t3)     = parseRel t4
      (a4, t4)     = parseE accepted tokens
      (a5, t5)     = parseFun [] (tail tokens)


parseFun :: [Expr] -> [String] -> ([Expr], [String])
parseFun accepted [] =  (accepted, [])
parseFun accepted (tok:tokens)
  | head tok == '}'  = (accepted, tokens)
  | otherwise        = parseFun (accepted ++ [a]) t
    where
      (a, t) = parseE (Val 0) (tok:tokens)

parseRel :: [String] -> (Relop, [String])
parseRel (tok:tokens)
  | (head tok) == '>' && (head tokens) == "=" = (">=", tail tokens)
  | (head tok) == '<' && (head tokens) == "=" = ("<=", tail tokens)
  | (head tok) == '<' && (head tokens) == ">" = ("<>", tail tokens)
  | (head tok) == '<'                         = ("<", tokens)
  | (head tok) == '>'                         = (">", tokens)
  | (head tok) == '='                         = ("=", tokens)


parseExpr :: String -> (Expr,[String])
parseExpr str = parseE (Val 0) (lexer str)


type Valuation = (Name, [Name], Expr)

parseProgram :: String -> ([Valuation], [Expr])
parseProgram str = (val, exps)
  where
    (val, toks) = parseValuation (lexer str)
    (exps) = parseExprs (toks)

parseValuation :: [String] -> ([Valuation], [String])
parseValuation [] = ([], [])
parseValuation (tok:tokens)
  | tok == "let" = ([(head (tokens), params, exps)] ++ val, r)
  | tok == "\n"  = parseValuation (tokens)
  | otherwise    = ([], tok:tokens)
    where
      (params, paramRest) = parseParameters (tail (tail (tail tokens)))
      (exps, rest) = parseE (Val 0) (paramRest)
      (val, r) = parseValuation (rest)

parseExprs :: [String] -> [Expr]
parseExprs [] = []
parseExprs (tok:tokens)
  | tok /= "\n" = let (expr, str) = parseE (Val 0) (tok:tokens)
      in [expr] ++ parseExprs str
  | otherwise = parseExprs tokens

parseParameters :: [String] -> ([Name], [String])
parseParameters [] = ([], [])
parseParameters (tok:tokens)
  | (head tok) == '^' = (([head tokens] ++ t), rest)
  | (head tok) == '-' = ([], tail tokens)
  | otherwise         = ([], tok:tokens)
    where
      (t, rest) = parseParameters (tail tokens)

checkSemantics :: ([Valuation],[Expr]) -> ([String], [String])
checkSemantics ([], []) = ([], [])
checkSemantics (vals, exprs) = ((checkValSem vals fNames [] fArg), (checkExprSem exprs (fNames, fArg) fArg))
  where
    fNames = map first vals
    fArg = map second vals
    first (x, _, _) = x
    second (_, x, _) = x

checkExprSem :: [Expr] -> ([Name], [[Name]]) -> [[Name]] -> [String]
checkExprSem [] _ _ = []
checkExprSem (e:expr) ([], []) [] = fetchWarning warnings
                                        ++ checkExprSem expr ([], []) []
  where
    fstWarning = let xs = [(checkIfNoDefORTooManyArgs e ([], []) [])] in
                  if length xs == 0
                    then []
                    else (head xs)
    warnings = [fstWarning] ++ [checkIfTooFewArgs e ([], [])]
    fetchWarning (w:warning) =
      if length w == 0
        then fetchWarning warning
        else w
checkExprSem (e:expr) (fNames, fArg) (vA:vArg) = fetchWarning warnings
                                        ++ checkExprSem expr (fNames, fArg) vArg
  where
    fstWarning = let xs = [(checkIfNoDefORTooManyArgs e (fNames, fArg) vA)] in
                  if length xs == 0
                    then []
                    else (head xs)
    warnings = [fstWarning] ++ [checkIfTooFewArgs e (fNames, fArg)]
    fetchWarning (w:warning) =
      if length w == 0
        then fetchWarning warning
        else w

checkIfTooFewArgs :: Expr -> ([Name], [[Name]]) -> [String]
checkIfTooFewArgs (Ident v expr) (fNames, fArg)
  | countArgs expr < (legalArgs v (fNames, fArg)) = ["Too few arguments for " ++ v]
  | otherwise                                     = ["OK"]
checkIfTooFewArgs (exp1 :+: exp2) (fNames, fArg) = (checkIfTooFewArgs exp1 (fNames, fArg)) ++ (checkIfTooFewArgs exp2 (fNames, fArg))
checkIfTooFewArgs (exp1 :-: exp2) (fNames, fArg) = (checkIfTooFewArgs exp1 (fNames, fArg)) ++ (checkIfTooFewArgs exp2 (fNames, fArg))
checkIfTooFewArgs (exp1 :/: exp2) (fNames, fArg) = (checkIfTooFewArgs exp1 (fNames, fArg)) ++ (checkIfTooFewArgs exp2 (fNames, fArg))
checkIfTooFewArgs (exp1 :*: exp2) (fNames, fArg) = (checkIfTooFewArgs exp1 (fNames, fArg)) ++ (checkIfTooFewArgs exp2 (fNames, fArg))
checkIfTooFewArgs (Val n) _  = []

checkValSem :: [Valuation] -> [Name] -> [Name] -> [[Name]] -> [String]
checkValSem [] _ _ _ = []
checkValSem (v:vals) fNames prevNames fArg = fetchWarning warnings
                                          ++ checkValSem vals fNames nextNames fArg
  where
    nextNames = prevNames ++ [f]
    f = first v
    expr = third v
    vNames = second v
    first (x, _, _) = x
    second (_, x, _) = x
    third (_, _, x) = x
    fstWarning = let xs = [(checkIfNoDefORTooManyArgs expr (fNames, fArg) vNames)] in
                  if length xs == 0
                    then []
                    else (head xs)

    sndWarning = checkIfUnDef f prevNames
    trdWarning = checkIfMultOcc f vNames
    warnings = [fstWarning] ++ [sndWarning] ++ [trdWarning]
    fetchWarning [] = ["OK"]
    fetchWarning (w:warning) =
      if length w == 0
        then fetchWarning warning
        else w

checkIfNoDefORTooManyArgs :: Expr -> ([Name], [[Name]]) -> [Name] -> [String]
checkIfNoDefORTooManyArgs (Ident v expr) (fNames, fArg) vNames
  | not (elem v fNames || elem v vNames)                                  = ["No definition of " ++ v]
  | not ((elem v vNames) || countArgs expr <= legalArgs v (fNames, fArg)) = ["Too many arguments for " ++ v]
  | otherwise                                                             = []
checkIfNoDefORTooManyArgs (exp1 :+: exp2) (fNames, fArg) vNames = pick (checkIfNoDefORTooManyArgs exp1 (fNames, fArg) vNames) (checkIfNoDefORTooManyArgs exp2 (fNames, fArg) vNames)
checkIfNoDefORTooManyArgs (exp1 :-: exp2) (fNames, fArg) vNames = pick (checkIfNoDefORTooManyArgs exp1 (fNames, fArg) vNames) (checkIfNoDefORTooManyArgs exp2 (fNames, fArg) vNames)
checkIfNoDefORTooManyArgs (exp1 :/: exp2) (fNames, fArg) vNames = pick (checkIfNoDefORTooManyArgs exp1 (fNames, fArg) vNames) (checkIfNoDefORTooManyArgs exp2 (fNames, fArg) vNames)
checkIfNoDefORTooManyArgs (exp1 :*: exp2) (fNames, fArg) vNames = pick (checkIfNoDefORTooManyArgs exp1 (fNames, fArg) vNames) (checkIfNoDefORTooManyArgs exp2 (fNames, fArg) vNames)
checkIfNoDefORTooManyArgs (Val n) _ _ = []

pick :: [String] -> [String] -> [String]
pick xs ys = if xs == []
  then ys
  else xs

countArgs :: [Expr] -> Int
countArgs expr = length expr

legalArgs :: Name -> ([Name], [[Name]]) -> Int
legalArgs _ ([], _) = -1
legalArgs fun (f:fs, a:args)
  | fun == f  = length a
  | otherwise = legalArgs fun (fs, args)

checkIfMultOcc :: Name -> [Name] -> [String]
checkIfMultOcc f [] = []
checkIfMultOcc f (v:vNames)
  | elem v vNames = ["Multiple occurrences of " ++ v ++ " in definition of " ++ f]
  | otherwise     = checkIfMultOcc f vNames

checkIfUnDef :: Name -> [Name] -> [String]
checkIfUnDef f prevNames = if (elem f prevNames)
                            then ["Redefinition of " ++ f]
                            else []


matchValId :: String -> [Expr] -> [Valuation] -> [Valuation] -> Integer
matchValId i args (v:vals) val
  | i == first v  = evalFunction (third v) (second v) (args) (val)
  | otherwise     = matchValId i args vals val
  where
    first (x, _, _) = x
    second (_, x, _) = x
    third (_, _, x) = x

matchLocalId :: Name -> [Name] -> [Expr] -> [Valuation] -> Expr
matchLocalId name (n:ns) (e:expr) val
  | name == n = e
  | otherwise = matchLocalId name ns expr val

evalFunction :: Expr -> [Name] -> [Expr] -> [Valuation] -> Integer
evalFunction (Ident name expr) localNames args val
  | elem name localNames = let ex = (matchLocalId name localNames args val)
                           in evalFunction ex localNames args val
  | otherwise = matchValId name e val val
  where
    e = argList expr
    argList [] = args
    argList (ex:expr) = [Val (evalFunction (ex) localNames args val)] ++ argList expr
evalFunction (Val n) _ _ val = n
evalFunction (Conditional lhs op rhs e0 e1) localNames args val =
  if (f op (evalFunction lhs localNames args val) (evalFunction rhs localNames args val))
    then evalFunction e0 localNames args val
    else evalFunction e1 localNames args val
      where
        f op lhs rhs
          | op == ">"  = (lhs) > (rhs)
          | op == "<"  = (lhs) < (rhs)
          | op == "="  = (lhs) == (rhs)
          | op == "<="  = (lhs) <= (rhs)
          | op == ">="  = (lhs) <= (rhs)
evalFunction (e0 :+: e1) localNames args val = (evalFunction e0 localNames args val) + evalFunction e1 localNames args val
evalFunction (e0 :-: e1) localNames args val = (evalFunction e0 localNames args val) - evalFunction e1 localNames args val
evalFunction (e0 :*: e1) localNames args val = (evalFunction e0 localNames args val) * evalFunction e1 localNames args val
evalFunction (e0 :/: e1) localNames args val = div (evalFunction e0 localNames args val) (evalFunction e1 localNames args val)
evalFunction (e0 :%: e1) localNames args val = mod (evalFunction e0 localNames args val) (evalFunction e1 localNames args val)


evalExpr :: Expr -> [Valuation] -> Integer
evalExpr (Val n) val = n
evalExpr (Ident name args) val = matchValId name args val val
evalExpr (Conditional lhs op rhs e0 e1) val =
  if (f op (evalExpr lhs val) (evalExpr rhs val))
    then evalExpr e0 val
    else evalExpr e1 val
      where
        f op lhs rhs
          | op == ">"  = (lhs) > (rhs)
          | op == "<"  = (lhs) < (rhs)
          | op == "="  = (lhs) == (rhs)
          | op == "<="  = (lhs) <= (rhs)
          | op == ">="  = (lhs) <= (rhs)
evalExpr (e0 :+: e1) val = (evalExpr e0 val) + evalExpr e1 val
evalExpr (e0 :-: e1) val = (evalExpr e0 val) - evalExpr e1 val
evalExpr (e0 :*: e1) val = (evalExpr e0 val) * evalExpr e1 val
evalExpr (e0 :/: e1) val = div (evalExpr e0 val) (evalExpr e1 val)
evalExpr (e0 :%: e1) val = mod (evalExpr e0 val) (evalExpr e1 val)


runProgram :: String -> [Integer]
runProgram str = helperRun exprs val
  where
    helperRun [] val = []
    helperRun (e:expr) val = [evalExpr e val] ++ helperRun expr val
    (val, exprs) = parseProgram str

isOk :: [String] -> Bool
isOk (x:xs) = if (x == "OK")
                then isOk xs
                else False
isOk [] = True
  
main = do
  contents <- getContents
  let semantic = checkSemantics (parseProgram contents)
  if (isOk (fst semantic)) && (isOk (snd semantic))
    then print (runProgram contents)
    else print (checkSemantics (parseProgram contents))
