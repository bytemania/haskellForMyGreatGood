module Monads where

import Control.Monad
import Data.Monoid
import Control.Monad.Writer
import System.Random
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.List
import Data.Ratio

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just a) f = f a

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n)  - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

(-:) :: a -> (a -> b) -> b
x -: f = f x

routine' :: Maybe Pole
routine' = case landLeft 1 (0,0) of
  Nothing -> Nothing
  Just pole1 -> case landRight 4 pole1 of
    Nothing -> Nothing
    Just pole2 -> case landLeft 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft 1 pole3

foo' :: Maybe String
foo' = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

routine'' :: Maybe Pole
routine'' = do
  start <- return (0,0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- return ""
  return x

listOfTuples'' :: [(Int, Char)]
listOfTuples'' = [(n, ch) | n <- [1,2], ch <- ['a', 'b']]

listOfTuples' :: [(Int, Char)]
listOfTuples' = [1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

listOfTuples = do
  n <- [1,2]
  ch <- ['a', 'b']
  return (n, ch)

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard('7' `elem` show x)
  return x

sevensOnly' :: [Int]
sevensOnly' = [x | x <- [1..50], '7' `elem` show x]

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1, r+2),(c-1,r-2),(c-1,r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = filter onBoard [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1, r+2),(c-1,r-2),(c-1,r+2)]
  where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first  <- moveKnight start
  second <- moveKnight first
  moveKnight second

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` (in3 start)

monadLawLeftIdentity :: Maybe Int
monadLawLeftIdentity = return 3 >>= \x -> Just (x + 1000000)

monadLawLeftIdentity' :: Maybe Int
monadLawLeftIdentity' = (\x -> Just (x + 1000000)) $ 3

monadLawRightIdentity :: Maybe String
monadLawRightIdentity = Just "move on up" >>= return

monadLawRightIdentity' :: Maybe String
monadLawRightIdentity' = Just "move on up"

monadLawAssociativity :: Maybe Pole
monadLawAssociativity = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

monadLawAssociativity' :: Maybe Pole
monadLawAssociativity' = ((return (0,0) >>= landRight 2) >>= landLeft 2) >>= landRight 2

monadLawAssociativity'' :: Maybe Pole
monadLawAssociativity'' = return (0,0) >>= (\x -> landRight 2 x >>= (\y -> landLeft 2 y >>= (\z -> landRight 2 z)))

composeMonadicFunctions :: [Int]
composeMonadicFunctions = let f x = [x,-x]
                              g x = [x*3,x*2]
                              h   = f <=< g
                          in h 3

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

applyLog :: Monoid m => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

gcd'' :: Int -> Int -> Int
gcd'' a b
  | b == 0 = a
  | otherwise = gcd'' b (a `mod` b)

gcd''' :: Int -> Int -> Writer [String] Int
gcd''' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd''' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

append :: (a -> b) -> (c -> a) -> c -> b
f `append` g = \xs -> f (g xs)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcd' b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x - 1)
  tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x - 1)
  tell [show x]

addStuff  :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

addStuff' :: Int -> Int
addStuff' x = let
  a = (*2) x
  b = (+10) x
  in (a + b)

threeCoins' :: StdGen -> (Bool, Bool, Bool)
threeCoins' gen = let
  (firstCoin, newGen) = random gen
  (secondCoin, newGen') = random newGen
  (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)

push' :: Int -> Stack -> ((), Stack)
push' a xs = ((),a:xs)

stackManip' :: Stack -> (Int, Stack)
stackManip' stack = let
  ((), newStack1) = push' 3 stack
  (a, newStack2) = pop' newStack1
  in pop' newStack2

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  pop
  pop

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5 then do
    push 5
  else do
    push 3
    push 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip
  if a == 100 then stackStuff else return ()

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
  then put [8,3,1]
  else put [9,2,1]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

runKeepSmall :: [Int]
runKeepSmall =  fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

runKeepSmallLog :: IO ()
runKeepSmallLog = mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True, False])

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

smallsTest :: Int -> Maybe Int
smallsTest n = foldM binSmalls 0 [1..n]

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return $ (x*y):ys
foldingFunction (x:y:ys) "+" = return $ (x+y):ys
foldingFunction (x:y:ys) "-" = return $ (y-x):ys
foldingFunction xs numberString = liftM (:xs) (readMaybe  numberString)

readMaybe :: Read a => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _         -> Nothing

mon1 :: (Monad m, Num a) => a -> m a
mon1 = return.(+1) <=< return.(*100)

testMon1 :: (Monad m, Num a) => m a -> m a
testMon1 m = m >>= mon1

calc' :: Int -> Int
calc' = foldr (.) id [(+1), (*100), (+1)]

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob [(Prob [('a',1%2),('b',1%2)], 1%4), (Prob [('c',1%2),('d',1%2)], 3%4)]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs,p) = map (\(x,r) -> (x, p*r)) innerxs

instance Applicative Prob where
  pure x = Prob [(x, 1%1)]
  Prob fs <*> Prob vs = Prob [(f v, pv) | (f,_) <- fs, (v,pv) <- vs]

instance Monad Prob where
  return x = Prob [(x, 1%1)]
  m >>= f = flatten (fmap f m)
  fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2), (Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10), (Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (==Tails) [a,b,c])
