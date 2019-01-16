module Functors where

import Data.Char
import Data.List
import Control.Applicative
import Data.Tuple
import Data.Monoid

main1 :: IO ()
main1 = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

main2 :: IO ()
main2 = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

main3 :: IO ()
main3 = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line

data CMaybe a = CNothing | CJust Int a deriving Show

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

myAction' :: IO String
myAction' = do
    a <- getLine
    b <- getLine
    return $ a ++ b

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

main4 :: IO ()
main4 = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (h:t) = (:) <$> h <*> sequenceA' t

sequenceA'' :: (Applicative f, Show a) => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])

data Profession = Fighter | Archer | Accountant
data Race = Human | Elf | Orc | Goblin
data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

data CoolBool' = CoolBool' {getCoolBool' :: Bool}

helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

newtype CoolBool = CoolBool {getCoolBool :: Bool}

helloMe  :: CoolBool -> String
helloMe (CoolBool _) = "hello"

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = let a = length x `compare` length y
                         b = x `compare` y
                    in if a == EQ then b else a

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

lengthCompare'' :: String -> String -> Ordering
lengthCompare'' x y = (length x `compare` length y) `mappend` (vowels x `compare` vowels y) `mappend` (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

