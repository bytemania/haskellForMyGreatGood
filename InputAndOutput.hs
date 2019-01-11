module InputAndOutput where

import Data.Char
import Control.Monad
import System.IO
import System.Directory
import Data.List
import System.Random

main0 = putStrLn "hello, world"

main1 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!"

main2 = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main3 = do
    line <- getLine
    if null line
    then return ()
    else do
        putStrLn $ reverseWords line
        main3

reverseWords :: String -> String
reverseWords = unwords.map reverse.words

main4 = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

main5 = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

main6 = do
    putStr "Hey, "
    putStr "I'm "
    putStrLn "Andy"

main7 = do
    putChar 't'
    putChar 'e'
    putChar 'h'

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (h:t) = do
    putChar h
    putStr' t

main8 = do
    print True
    print 2
    print "haha"
    print 3.2
    print [1..10]

main9 = do
    c <- getChar
    if c /= ' ' then do
        putChar c
        main9
    else return ()

main10 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main10

main11 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

main12 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

main13 = sequence $ map print [1..5]

main14 = mapM print [1..5]

main15 = mapM_ print [1..5]

main16 = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

main17 = do
    colors <- forM [1..4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM_ putStrLn colors

main18 = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let
        allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result

main19 = interact shortLinesOnly

main20 = interact $ unlines . filter ((< 10) . length) . lines


responsePalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where isPalindrome xs = xs == reverse xs

main21 = interact responsePalindromes

main22 = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

main23 = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStr contents
        )

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

main24 = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlFriendCaps.txt" $ map toUpper contents

main25 = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

main26 = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents
     )

main27 = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"



