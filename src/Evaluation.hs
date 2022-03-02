module Evaluation 
    (    parsing,
        whileEval,
        repl) where

import Element
import Functions
import System.Console.Haskeline
import Parser
import Tools

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


whileEval :: [Element] -> Env -> (Element, Env)
whileEval elements env | rest == [] = (ret, nenv)
                       | ret == Error = (ret, env)
                       | otherwise = case isSymbols ret of 
                        False -> whileEval (ret : rest) nenv
                        otherwise -> whileEval (rest) nenv
                       where
                           (ret, rest, nenv) = eval elements env

repl :: Env -> InputT IO ()
repl env = do
    str <- getInputLine "> "
    case str of
        Just "quit" -> return ()
        Nothing -> return ()
        Just str -> do
            let parsedElement = parseCons $ dropPar (parsing str)
            let (sexpr, nenv) = (whileEval parsedElement env)
            outputStrLn $ show $ sexpr
            repl nenv
