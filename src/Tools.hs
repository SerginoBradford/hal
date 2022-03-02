module Tools 
    (   appendSexpr,
        convertToString,
        appendSexprTo,
        printSexpr,
        isNber,
        isSymbols,
        dropPar,
        appendAllFileContent) where

import Element

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

isSymbols :: Element -> Bool
isSymbols (Symbols s) = True
isSymbols elements = False

isNber :: Element -> Bool
isNber (Number s) = True
isNber elements = False

dropPar :: [Element] -> [Element]
dropPar elements | (head elements) == OpenCons && (last elements) == CloseCons = init $ tail elements
                 | otherwise = elements

appendAllFileContent :: [String] -> String
appendAllFileContent [] = []
appendAllFileContent (x:xs) = x ++ (appendAllFileContent xs)
