module Element 
    (   Element(..),
        Env(..)) where


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

type Env = [(Element, Element)]
