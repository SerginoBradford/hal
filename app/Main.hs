module Main where

import Lib
import Parser
import Element
import Tools
import Evaluation

import Data.Char
import Data.Maybe
import Data.List
import Text.Printf

import System.Environment
import System.IO
import System.Exit
import System.Console.Haskeline


main :: IO ()
main = do
        arg <- getArgs
        let (args, i) = if (last arg) == "-i" then ((init arg), 1) else (arg, 0)
        fileContent <- mapM readFile args
        let elements = appendAllFileContent (fileContent)
        let parsedElement = parseCons $ dropPar $ parsing elements
        let (sexpr, env) = (whileEval parsedElement [])
        print sexpr
        if i == 1
        then (runInputT defaultSettings (repl env))
        else return ()