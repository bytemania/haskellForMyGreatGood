module Random where

import System.Random
import Control.Monad(when)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import System.Environment

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = let
        (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)

main1 = do
    gen <- getStdGen
    putStrLn  $ take 20 (randomRs ('a', 'z') gen)
    gen2 <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen2)

main2 = do
    gen <- getStdGen
    let randomChars = randomRs('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _)   = splitAt 20 rest
    putStrLn first20
    putStrLn second20

main3 = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)
    gen' <- newStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen')

main4 = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number then putStrLn "You are correct!" else putStrLn $ "Sorry, it was " ++ (show randNumber)
        askForNumber newGen

main5 = do
    gen <- getStdGen
    let (randomNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randomNumber == number
        then putStrLn "You are correct!"
        else putStrLn $ "Sorry, it was " ++ (show randomNumber)
    newStdGen
    main5



