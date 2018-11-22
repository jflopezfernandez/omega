
import System.IO


main :: IO ()
main = do
    putStrLn "<Omega Compiler Whitespace Length Analysis>"
    putStr "Enter string: "
    hFlush stdout
    line <- getLine
    if (length line) == 0 then
            return ()
    else do
        putStrLn $ "Length: " ++ (show (length line))
        main
