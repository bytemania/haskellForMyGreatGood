module Person where

import Data.List
import qualified Data.Map as Map

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Eq, Show, Read)

mikeD :: Person
mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}

adRock :: Person
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}

mca :: Person
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

beastieBoys :: [Person]
beastieBoys = [mca, adRock, mikeD]

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)



type AssocList k v = [(k, v)]

type PhoneNumber = String
type Name = String
type PhoneBook = AssocList Name PhoneNumber

phoneBook :: PhoneBook
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

getAssocList :: Eq k => k -> AssocList k v -> Maybe v
getAssocList k  = fmap snd . find ((==) k . fst)

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = elem (name, pnumber) pbook

type IntMap = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker Number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

data ListRecord a = EmptyRecord | ConsRecord {listHead ::a , listTail :: ListRecord a}
    deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: List a deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node v left right)
    | x == v    = Node x left right
    | x <= v    = Node v (treeInsert x left) right
    | otherwise = Node v left (treeInsert x right)

numsTree :: Tree Int
numsTree = foldr treeInsert EmptyTree [8, 6, 4, 1, 7, 3, 5]

treeElem :: Ord a => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node v l r)
    | x == v    = True
    | x < v     = treeElem x l
    | otherwise = treeElem x r

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving Show

instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
    fmap f (Barry {yabba = a, dabba = y}) = Barry {yabba = f a, dabba = y}


