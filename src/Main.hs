
import Data.Char
import System.IO

showLength :: String -> String
showLength str = show (length str)

data Operator = Add
              | Subtract
              | Multiply
              | Divide
    deriving (Show, Eq)

data Token = TokenOp Operator
           | TokenNum Int
           | TokenEnd
    deriving (Show, Eq)

operator :: String -> Operator
operator str
    | str == "\t"           = Add
    | str == "\t\t"         = Subtract
    | str == "\t\t\t"       = Multiply
    | str == "\t\t\t\t"     = Divide
    | otherwise             = error $ "<Other Op>: " ++ (show (length str))

accumulateTabs :: String -> String -> [Token]
accumulateTabs acc [] = [TokenOp (operator acc)]
accumulateTabs acc (x:xs)
    | [x] == "\t" = accumulateTabs (acc ++ [x]) xs
    | otherwise = TokenOp (operator acc) : tokenize (x:xs)

accumulateSpaces :: String -> String -> [Token]
accumulateSpaces acc [] = [(number acc)]
accumulateSpaces acc (x:xs)
    | [x] == " " = accumulateSpaces (acc ++ [x]) xs
    | otherwise = number acc : tokenize (x:xs)

number :: String -> Token
number str = TokenNum (length str)

tokenize :: String -> [Token]
tokenize "" = []
tokenize (x:xs)
    | [x] == "\t" = accumulateTabs [x] xs
    | [x] == " " = accumulateSpaces [x] xs
    | otherwise = error "Other"

data Tree = NumberNode Int
          | UnaryNode Operator Tree
          | SumNode Operator Tree Tree
          | ProductNode Operator Tree Tree
    deriving (Show, Eq)

lookAhead :: [Token] -> Token
lookAhead [] = TokenEnd
lookAhead (c:_) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (_:ts) = ts

expression :: [Token] -> (Tree, [Token])
expression tokens =
    let
        (termTree, tokens') = term tokens
    in
        case lookAhead tokens' of
            (TokenOp op) | elem op [Add, Subtract] ->
                let
                    (expressionTree, tokens'') = expression (accept tokens')
                in
                    (SumNode op termTree expressionTree, tokens'')
            
            _ -> (termTree, tokens')

term :: [Token] -> (Tree, [Token])
term tokens =
    let
        (factorTree, tokens') = factor tokens
    in
        case lookAhead tokens' of
            (TokenOp op) | elem op [Multiply, Divide] ->
                let
                    (termTree, tokens'') = term (accept tokens')
                in
                    (ProductNode op factorTree termTree, tokens'')
            
            _ -> (factorTree, tokens')

factor :: [Token] -> (Tree, [Token])
factor tokens =
    case lookAhead tokens of
        (TokenNum n) -> (NumberNode n, accept tokens)

        (TokenOp op) | elem op [Add, Subtract] ->
            let
                (factorTree, tokens') = factor (accept tokens)
            in
                (UnaryNode op factorTree, tokens')

        _ -> error $ "Parse error on token: " ++ show tokens

parse :: [Token] -> Tree
parse tokens =
    let
        (tree, tokens') = expression tokens
    in
        if null tokens' then
            tree
        else
            error $ "Leftover tokens: " ++ show tokens'
        
loop :: IO ()
loop = do
    line <- getLine
    if (length line) == 0 then
        return ()
    else do
        (print . tokenize)          (line :: String)
        (print . parse . tokenize)  (line :: String)
        main


main :: IO ()
main = do
    loop
