
import System.IO

loop :: IO ()
loop = do
    putStr "Enter string: "
    hFlush stdout
    line <- getLine
    if (length line) == 0 then
        return ()
    else do
        putStrLn $ "Length: " ++ (show (length line))
        loop


main :: IO ()
main = do
    putStrLn "<Omega Compiler Whitespace Length Analysis>"
    loop
