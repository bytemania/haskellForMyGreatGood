module Types where

factorial :: Int -> Int
factorial n = product [1..n]

circumference :: Double -> Double
circumference r = 2 * pi * r

lucky :: Integral a => a -> String
lucky 7 = "LUCKY NUMBER 7!"
lucky x = "Sorry, you're out of luck pal!"

sayMe :: Integral a => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

factorial' :: Integral a => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n -1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: Integral a => (a, a) -> (a, a) -> (a, a)
addVectors a b  = (fst a + fst b, snd a + snd b)

addVectors' :: Integral a => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

head' :: [a] -> a
head' [] = error "Can't call head of an empty list, dummy!"
head' (h:_) = h

tell :: (Show a) => [a] -> String
tell []       = "The list is empty"
tell (x:[])   = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements " ++ show x ++ " and " ++ show y
tell (x:y:_)  = "The list is too long. The first two elements are: " ++ show x ++ " and " ++ show y


length' :: Num b => [a] -> b
length' []    = 0
length' (_:t) = 1 + length' t

sum' :: Num a => [a] -> a
sum' []    = 0
sum' (h:t) = h + sum' t

capital :: String -> String
capital []        = "Empty string, whoops!"
capital all@(h:t) = "The first letter of " ++ all ++ " is " ++ [h]

bmiTell' :: RealFloat a => a -> String
bmiTell' bmi
    | bmi <= 18.5 = "You're underweigth, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're uggly"
    | bmi <= 30.0 = "You're fat! Loose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

bmiTell :: RealFloat a => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweigth, you emo, you!"
    | weight / height ^ 2 <= 25.0  = "You're supposedly normal. Pffft, I bet you're uggly"
    | weight / height ^ 2 <= 30.0  = "You're fat! Loose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

max' :: Ord a => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: Ord a => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a < b = LT
    | otherwise = EQ

bmiTell'' :: RealFloat a => a -> a -> String
bmiTell'' weight height
        | bmi <= skinny = "You're underweigth, you emo, you!"
        | bmi <= normal = "You're supposedly normal. Pffft, I bet you're uggly"
        | bmi <= fat    = "You're fat! Loose some weight, fatty!"
        | otherwise     = "You're a whale, congratulations!"
        where
            bmi = weight / height ^ 2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
    where
        (f:_) = firstName
        (l:_) = lastName

calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: RealFloat a => a -> a -> a
cylinder r h =
    let
        sideArea = 2 * pi + r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

num0 = 4 * (let a = 9 in a + 1) + 2

res0 = [let square x = x * x in (square 5, square 3, square 2)]

res1 = (let a = 100; b = 200; c = 300 in a * b * c, let foo = "Hey "; bar = "there!" in foo ++ bar)

res2 = (let (a, b, c) = (1, 2, 3) in a + b + c) * 100

calcBmisList :: RealFloat a => [(a, a)] -> [a]
calcBmisList xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmisFat :: RealFloat a => [(a, a)] -> [a]
calcBmisFat xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25]

head'' :: [a] -> a
head'' xs = case xs of
    [] -> error "No head for empty lists!"
    (h : _) -> h

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
    []        -> "empty."
    [x]       -> "a singleton list."
    otherwise -> "a longer list."

describeList''' :: [a] -> String
describeList''' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."