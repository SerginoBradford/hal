module Main where

import Lib
import Parser

import Data.Char
import Data.Maybe
import Data.List
import Text.Printf

import System.Environment
import System.IO
import System.Exit

import System.Console.Haskeline

data Element = Function String
            | Symbols String
            | Cons [Element]
            | Number Int
            | Operator Char
            | Result String
            | Lambda (Element, Element)
            | OpenCons
            | CloseCons
            | Nil
            | Dot
            | Error
            | End
             deriving (Eq)

type Env = [(Element, Element)]

data EltParser a = EltParser {
    runEltParser :: [Element] -> Maybe (a, [Element])
}

instance Show Element where
        show (Number x) = show x
        show (Symbols x) = x
        show (Lambda x) = "#procedure"
        show (Cons c) = "(" ++ unwords (map show c) ++ ")"
        show (Result x) = x
        show (Function x) = x            
        show OpenCons = "("
        show CloseCons = ")"
        show Nil = "()"
        show Dot = "."
        show Error = "Error"
        show End = "End"

parsing :: String -> [Element]
parsing [] = []
parsing str | take 2 str == "()" = [Nil] ++ parsing (drop 2 str)
            | (head str) == '.' = [Dot] ++ parsing (tail str)
            | runParser (parseAnyChar "'-+*<") str /= Nothing = (Function ([head str])) : parsing (tail str)
            | (head str) == '(' =  [OpenCons] ++ parsing (tail str)
            | (head str) == ')' = [CloseCons] ++ parsing (tail str)
            | runParser (parseSome (parseAnyChar (['a'..'z'] ++ "#?"))) str /= Nothing = 
            case a of 
                x | elem x ["div", "let", "mod", "atom?", "cdr", "car", "define", "eq?", "cons", "cond", "lambda", "quote"] -> ([Function a] ++ parsing b)
                x | elem x ["#t", "#f"] -> ([Result x] ++ parsing b)
                otherwise -> ([Symbols a] ++ parsing b)
            | runParser parseInt str /= Nothing = [Number x] ++ parsing y
                where
                    Just (a, b) = runParser (parseSome (parseAnyChar (['a'..'z'] ++ "#?"))) str
                    Just (x, y) = runParser parseInt str
parsing str = parsing (tail str)

checkCons :: EltParser [Element]
checkCons = EltParser $ \s -> case s of
    [] -> Nothing
    elements ->
        if (head elements) == OpenCons then Just ([Cons (result)] ++ result1, rest1)
        else
            if (head elements) == CloseCons then Just ([], (tail elements))
            else  Just (([head elements] ++ result), rest)
            where
                Just (result, rest) = runEltParser checkCons (tail elements)
                Just (result1, rest1) = runEltParser checkCons rest


parseCons :: [Element] -> [Element]
parseCons elements | length elements == 0 = elements
                   | head elements == OpenCons = Cons result : parseCons rest
                   | otherwise = (head elements) : (parseCons $ tail elements)
                   where
                       Just (result, rest) = runEltParser checkCons (tail elements)


appendSexpr :: [Element] -> String
appendSexpr [] = []
appendSexpr ((Number a):xs) = show (a) ++  " " ++ (appendSexpr xs)
appendSexpr ((Symbols s):xs) = s ++ " " ++ (appendSexpr xs)
appendSexpr ((Cons (a:b:[])):xs) = "(" ++ appendSexpr [a] ++ ". " ++ take ((length (appendSexpr [b])) - 1) (appendSexpr [b]) ++ ") " ++ (appendSexpr xs)
appendSexpr elements = appendSexpr (tail elements)

convertToString :: Element -> String
convertToString element = show element

appendSexprTo :: [Element] -> String
appendSexprTo [] = "()"
appendSexprTo (OpenCons:xs) = appendSexprTo xs ++ ")" 
appendSexprTo ((Number a):xs) = "(" ++ show a ++ " . " ++ appendSexprTo xs ++ ")"
appendSexprTo ((Cons (a:b:[])):xs) = "((" ++ appendSexpr [a] ++ ". " ++ take ((length (appendSexpr [b])) - 1) (appendSexpr [b]) ++ ") ." ++ appendSexprTo xs ++ ")"
appendSexprTo elements = appendSexprTo $ tail elements

printSexpr :: String -> IO ()
printSexpr elements = putStrLn $ id elements

car:: Element -> Element
car (Cons []) = Cons []
car (Cons elements) = head elements

cdr:: Element -> Element
cdr (Cons elements) = (Cons $ tail elements)

cons :: Element -> Element -> Element
cons (Cons a) (Cons b) = Cons $ a ++ b
cons (Cons a) b = Cons $ a ++ [b]
cons a (Cons b) = Cons $ [a] ++ b
cons a b = Cons $ [a] ++ [b]

quote :: [Element] ->  Element
quote elements = head elements

mySum :: [Element] -> Env -> Int
mySum [] env = 0
mySum ((Symbols s):elements) env = (a + (myMul elements env))
                            where (Number a) = getSymbValue env (Symbols s)
mySum ((Number a):elements) env = (a + (mySum elements env))
mySum ((Cons c):elements) env = (a + (mySum elements env))
        where
            (fa, s, q) = eval c env
            (Number a, lres, lenv) = eval [fa] q

mySous :: [Element] -> Env -> Int
mySous [] env = 0
mySous ((Symbols s):elements) env = (a - (myMul elements env))
                                where
                                    (Number a) = getSymbValue env (Symbols s)
mySous ((Number a):elements) env = (a - (mySous elements env))
mySous ((Cons c):elements) env = (a - (mySous elements env))
        where
            (fa, s, q) = eval c env
            (Number a, lres, lenv) = eval [fa] q

myMul :: [Element] -> Env -> Int
myMul [] env = 1
myMul ((Symbols s):elements) env = (a * (myMul elements env))
                                where
                                    (Number a) = getSymbValue env (Symbols s)
myMul ((Number a):elements) env = (a * (myMul elements env))
myMul ((Cons c):elements) env = (a * (myMul elements env))
        where
            (fa, s, q) = eval c env
            (Number a, lres, lenv) = eval [fa] q

{-myMul :: [Element] -> Env -> Int
myMul [] env = 1
myMul ((Number a):elements) env = a * (myMul elements env)
myMul ((Symbols s):elements) env = a * (myMul elements env)
                                where
                                    ret = getSymbValue env (Symbols s)
                                    a = case isNber ret of
                                        True -> let (Number value) = ret in value
                                        False -> let (Number value,lres1, lenv) = eval ([ret] ++ elements )env in value
myMul ((Cons c):elements) env = a * (myMul elements env)
        where
            (fa, s, q) = eval c env
            (ret, lres, lenv) = eval [fa] q
            a = case isNber ret of
                True -> let (Number value) = ret in value
                False -> let (Number value, lres1, lenv) = eval ([ret] ++ elements) lenv in value
-}
myDiv :: Element -> Element -> Int
myDiv (Number a) (Number b) = div a b

myMod :: Element -> Element -> Int
myMod (Number a) (Number b) = mod a b

myInf :: Element -> Element -> String
myInf (Number a) (Number b) | a < b = "#t"
                            | otherwise = "#f"

myEq :: Element -> Element -> String
myEq (Cons a) (Cons b) = "#f"
myEq a b | show a == show b = "#t"
         | otherwise = "#f"

myAtom :: Element -> String
myAtom (Cons a) = "#f"
myAtom elem = "#t"

getSymbValue :: Env -> Element -> Element
getSymbValue [] elem = Error
getSymbValue ((symbols, value):xs) elem | elem == symbols = value
                                        | otherwise = getSymbValue xs elem

addToEnv :: Env -> Element -> Element -> Env
addToEnv env s v = env ++ [(s, v)]

mkLambda :: [Element] -> (Element, [Element])
mkLambda (x:xs) = (Lambda (x, head xs), tail xs)

defLambda :: [Element] -> [Element] -> Env -> Env
defLambda [] _ env = [(Nil, Nil)]
defLambda arg elements env = [(head arg, a)] ++ defLambda (tail arg) (tail elements) env
                                    where
                                    (a, rest, env) = eval elements env

defLet :: Element -> Env
defLet (Cons ((Cons (x:xs)):(Cons (y:ys)):[] ) ) = [(x, a)] ++ [(y, b)]
                                where
                                    (a, rest, env) = eval xs []
                                    (b, rest1, env1) = eval ys env

checkCond :: [Element] -> Env -> Element
checkCond [] env = Error
checkCond ((Cons (elements)):xs) env | ret == Result "#t" = ret1
                       | otherwise = checkCond xs env
                        where
                            (ret, rest, nenv) = eval elements env
                            (ret1, rest1, nenv1) = eval rest nenv
checkCond elements env = Error

higherFunctions :: [Element] -> Env -> (Element, [Element], Env)
higherFunctions ((Function f):xs) env | f == "let" = eval [head $ tail xs] (defLet (head xs))
                                      | f == "define" = ((head xs), [], (addToEnv env (head xs) (head $ tail xs)))
                                      | f == "lambda" = (lambda, rest2, env)
                                      | f == "cond" = ((checkCond xs env), [], env)
                                    where
                                        (first, rest, nenv) = eval xs env
                                        (second, rest1, nenv1) = eval rest env
                                        (lambda, rest2) = mkLambda xs

compFunctions :: [Element] -> Env -> (Element, [Element], Env)
compFunctions ((Function f):xs) env | f == "cons" = (cons first second, rest1, env)
                                    | f == "car" = (car first, rest, nenv)
                                    | f == "cdr" = (cdr first, rest, nenv)
                                    | f == "atom?" = (Result $ myAtom first, rest, env)
                                    | f == "eq?" = (Result (myEq first second), rest1, nenv1)
                                    | f == "<" = (Result (myInf first second), rest1, nenv1)
                                    | otherwise = higherFunctions ((Function f):xs) env
                                    where
                                        (first, rest, nenv) = eval xs env
                                        (second, rest1, nenv1) = eval rest env

arithFunctions :: [Element] -> Env -> (Element, [Element], Env)
arithFunctions ((Function f):xs) env | f == "*" = (Number (myMul xs env), [], env)
                                     | f == "+" = (Number (mySum xs env), [], env)
                                     | f == "'" || f == "quote" = ((head xs), (tail xs), env)
                                     | f == "-" = (Number (mySous xs env), [], env)
                                     | f == "div" = (Number (myDiv first second), rest1, nenv1)
                                     | f == "mod" = (Number (myMod first second), rest1, nenv1)
                                     | otherwise = compFunctions ((Function f):xs) env
                                    where
                                        (first, rest, nenv) = eval xs env
                                        (second, rest1, nenv1) = eval rest env

allFunctions :: [Element] -> Env -> (Element, [Element], Env)
allFunctions ((Function f):xs) env = arithFunctions ((Function f):xs) env 

allSymbols :: [Element] -> Env -> (Element, [Element], Env)
allSymbols ((Symbols s):xs) env = if isNber (getSymbValue env (Symbols s)) == True
    then (getSymbValue env (Symbols s), xs, env)
    else eval ((getSymbValue env (Symbols s)):xs) env

eval :: [Element] -> Env -> (Element, [Element], Env)
eval [] env = (End, [], env)
eval ((Symbols s):xs) env = allSymbols ((Symbols s):xs) env 
eval ((Result r):xs) env = (Result r, xs, env)
eval ((Number n):xs) env = (Number n, xs, env)
eval ((Cons sub):xs) env = (result, xs, nenv)
                            where
                              (result, b, nenv) = eval sub env
eval ((Lambda (Cons arg, Cons func)):xs) env = (ret, rest, env)
                                                where
                                                    (ret, rest, nenv) = eval func ((defLambda arg xs env) ++ env)
eval element env = allFunctions element env

isSymbols :: Element -> Bool
isSymbols (Symbols s) = True
isSymbols elements = False

isNber :: Element -> Bool
isNber (Number s) = True
isNber elements = False

whileEval :: [Element] -> Env -> (Element, Env)
whileEval elements env | rest == [] = (ret, nenv)
                       | ret == Error = (ret, env)
                       | otherwise = case isSymbols ret of 
                        False -> whileEval (ret : rest) nenv
                        otherwise -> whileEval (rest) nenv
                       where
                           (ret, rest, nenv) = eval elements env

appendAllFileContent :: [String] -> String
appendAllFileContent [] = []
appendAllFileContent (x:xs) = x ++ (appendAllFileContent xs)

repl :: Env -> InputT IO ()
repl env = do
    str <- getInputLine "> "
    case str of
        Just "quit" -> return ()
        Nothing -> return ()
        Just str -> do
            let parsedElement = parseCons $ dropPar (parsing str)
            let (sexpr, nenv) = (whileEval parsedElement env)
            --let result = convertToString sexpr
            outputStrLn $ show $ sexpr --result
            repl nenv

dropPar :: [Element] -> [Element]
dropPar elements | (head elements) == OpenCons && (last elements) == CloseCons = init $ tail elements
                 | otherwise = elements

main :: IO ()
main = do
        arg <- getArgs
        let (args, i) = if (last arg) == "-i" then ((init arg), 1) else (arg, 0)
        fileContent <- mapM readFile args
        let elements = appendAllFileContent (fileContent)
        let parsedElement = parseCons $ dropPar $ parsing elements
        let (sexpr, env) = (whileEval parsedElement [])
        --let result = convertToString sexpr
        --printSexpr result
        print sexpr
        if i == 1
        then (runInputT defaultSettings (repl env))
        else return ()